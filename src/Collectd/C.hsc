{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}

module Collectd.C where

import           Foreign
import           Foreign.C.Types
import           Foreign.C.String

import           Data.Word                ( Word8(..) )

import           Collectd.Internal
import           Collectd.Types

import qualified Data.Map                as Map
import           Control.Monad           (forM, mapM)
import           Control.Applicative

#include <collectd.h>
#include <plugin.h>
#include <liboconfig/oconfig.h>
#include <custom.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-- |
-- | Data Types
-- |

data DataSet = DataSet
    { dstType :: !String
    , dstDs   :: ![DataSource]
    } deriving(Eq, Show)

data DataSource = DataSource
    { dsName :: !String
    , dsType :: !Int         -- | 0 - Counter | 1 - Gauge | 2 - Derive | 3 - Absolute
    , dsMin  :: !Double
    , dsMax  :: !Double
    } deriving (Eq, Show)

type DataSetPtr = Ptr DataSet
type DataSourcePtr   = Ptr DataSource

instance Storable DataSet where
  alignment _ = #{alignment data_set_t}
  sizeOf _    = #{size data_set_t}
  peek p = do
    DataSet
      `fpStr` #{ptr data_set_t, type} p
      `apArr` (#{peek data_set_t, ds_num} p,
               #{peek data_set_t, ds}     p)

  poke ptr (DataSet a ds) = undefined

instance Storable DataSource where
  alignment _ = #{alignment data_source_t}
  sizeOf _    = #{size      data_source_t}

  peek p      = do
    DataSource
      `fpStr` #{ptr data_source_t, name}  p
      `apInt` #{peek data_source_t, type} p
      `apDbl` #{peek data_source_t, min}  p
      `apDbl` #{peek data_source_t, max}  p
  poke p      = undefined

type FreeFn = Ptr () -> IO ()

data UserData = UserData
    { udData    :: Ptr ()
    , udFreeFn  :: FunPtr FreeFn
    } deriving(Eq, Show)

instance Storable UserData where
  alignment _ = #{alignment user_data_t}
  sizeOf _    = #{size      user_data_t}

  peek p      = do
    dataPtr <- #{peek user_data_t, data} p
    freeFn  <- #{peek user_data_t, free_func} p
    return $ UserData dataPtr freeFn

  poke p UserData{..} = do
    #{poke user_data_t, free_func} p udFreeFn
    #{poke user_data_t, data} p udData

type UserDataPtr     = Ptr UserData

type CollectdValuePtr =  Ptr ()

unpackValue :: (Int, CollectdValuePtr) -> IO CollectdValue
unpackValue (tp, ptr) =
  case tp of
    0 -> CounterT  <$> mkInt <$> peek (castPtr ptr)
    1 -> GaugeT    <$> mkDbl <$> peek (castPtr ptr)
    2 -> DeriveT   <$> mkInt <$> peek (castPtr ptr)
    3 -> AbsoluteT <$> mkInt <$> peek (castPtr ptr)

data RawValueList = RawValueList
    { vlHost           :: !String
    , vlPlugin         :: !String
    , vlPluginInstance :: !String
    , vlType           :: !String
    , vlTypeInstance   :: !String
    -- , vlMetaData       :: !String
    , vlTime           :: !Int
    , vlInterval       :: !Int
    , vlValues         :: ![CollectdValuePtr]
    } deriving(Eq, Show)

unpackValueList :: DataSetPtr -> RawValueListPtr -> IO ValueList
unpackValueList dataSetPtr rawValueListPtr = do
  DataSet{..}      <- peek dataSetPtr
  RawValueList{..} <- peek rawValueListPtr
  let valuePointerPairs = zipWith (,)
                          (map dsType dstDs)
                          vlValues
  values <- mapM unpackValue valuePointerPairs
  return $ ValueList
    vlPlugin
    vlPluginInstance
    vlType
    vlTypeInstance
    vlHost
    vlInterval
    vlTime
    (Map.fromList (zipWith (,) (map dsName dstDs) values))


instance Storable RawValueList where
  alignment _ = #{alignment value_list_t}
  sizeOf _    = #{size      value_list_t}

  peek p      = do
    valuesLenVal <- (#{peek value_list_t, values_len} p) :: IO CInt
    valuesPtr    <- #{peek value_list_t, values} p
    let valuesLen    = fromIntegral valuesLenVal
        getValueAt i = plusPtr valuesPtr (#{offset value_list_t, values} + #{size value_t} * i)
        valuePtrs    = map getValueAt [0..(valuesLen - 1)]
        valuePtrsIO  = return valuePtrs

    RawValueList
      `fpStr` #{ptr   value_list_t, host}             p
      `apStr` #{ptr   value_list_t, plugin}           p
      `apStr` #{ptr   value_list_t, plugin_instance}  p
      `apStr` #{ptr   value_list_t, type}             p
      `apStr` #{ptr   value_list_t, type_instance}    p
      `apInt` #{peek  value_list_t, time}             p
      `apInt` #{peek  value_list_t, interval}         p
      <*>     valuePtrsIO


type RawValueListPtr    = Ptr RawValueList

data ConfigValue = ConfigValueString String |
                   ConfigValueDouble Double |
                   ConfigValueBool   Bool
                 deriving(Eq, Show)
instance Storable ConfigValue where
  alignment _ = #{alignment oconfig_value_t}
  sizeOf _    = #{size oconfig_value_t}
  peek p      = do
    unionType  <- #{peek oconfig_value_t, type} p

    case (mkInt unionType) of
      0 -> do
        unionValue <- #{peek oconfig_value_t, value} p

        ConfigValueString <$>
          (peekCString $ #{ptr oconfig_value_t, value}  unionValue)
      1 -> ConfigValueDouble <$>
           (mkDbl <$> #{peek oconfig_value_t, value} p)

      2 -> do
        v <- mkInt <$> #{peek oconfig_value_t, value} p
        return $ ConfigValueBool $ case v of
          0 -> False
          1 -> True

data ConfigItem = ConfigItem
    { cfgItemKey      :: !String
    , cfgItemValues   :: ![ConfigValue]
    , cfgItemChildren :: ![ConfigItem]
    } deriving(Eq, Show)

type ConfigItemPtr  = Ptr ConfigItem

instance Storable ConfigItem where
  alignment _ = #{alignment oconfig_item_t}
  sizeOf _    = #{size oconfig_item_t}
  peek p      = do
    keyPtr         <- #{peek oconfig_item_t, key} p
    key            <- peekCString keyPtr
    numberOfValues <- mkInt <$> #{peek oconfig_item_t, values_num} p
    numberOfItems  <- mkInt <$> #{peek oconfig_item_t, children_num} p

    valuesPtr      <- #{peek oconfig_item_t, values} p
    values         <- peekArray numberOfValues valuesPtr

    childrenPtr    <- #{peek oconfig_item_t, children} p
    children       <- peekArray numberOfItems childrenPtr

    return $ ConfigItem key values children


data Custom = Custom
    { customName   :: !String
    , customI      :: !Int
    } deriving(Eq, Show)

type CustomPtr = Ptr Custom

instance Storable Custom where
  alignment _ = #{alignment custom_t}
  sizeOf _    = #{size      custom_t}

  peek p      = do
    Custom
      `fpStr` #{ptr custom_t, name}  p
      `apInt` #{peek custom_t, i} p

  poke p Custom{..} = do
    cCustomName     <- newCString customName
    customNameValue <- peekArray (length customName) cCustomName
    pokeArray (#{ptr custom_t, name} p) customNameValue

    #{poke custom_t, i} p customI


makeCustom :: Custom -> IO (Ptr Custom)
makeCustom c = do
  customMem <- calloc
  _         <- poke customMem c

  return $ customMem

type CallbackFn      = CString -> CString -> CInt
type ConfigFn        = ConfigItemPtr -> CInt

type ConfigCallbackFn =
  ConfigItemPtr
  -> IO CInt

type WriteCallbackFn  =
  DataSetPtr
  -> RawValueListPtr
  -> UserDataPtr
  -> IO CInt

foreign import ccall safe "collectd/plugin.h plugin_register_config"
  plugin_register_config ::
    CString
    -> FunPtr CallbackFn
    -> FunPtr CallbackFn
    -> Ptr CString
    -> IO CInt

foreign import ccall safe "collectd/plugin.h plugin_register_complex_config"
  plugin_register_complex_config ::
    CString
    -> FunPtr ConfigCallbackFn
    -> IO CInt

foreign import ccall safe "collectd/plugin.h plugin_register_write"
  plugin_register_write ::
    CString
    -> FunPtr WriteCallbackFn
    -> UserDataPtr
    -> IO CInt

-- |
-- | Wrapper funcitons for converting Haskell functions into C callbacks
-- |
foreign import ccall safe "wrapper"
  makeWriteCallbackFn  :: WriteCallbackFn  -> IO (FunPtr WriteCallbackFn)

foreign import ccall safe "wrapper"
  makeConfigCallbackFn :: ConfigCallbackFn -> IO (FunPtr ConfigCallbackFn)

foreign import ccall safe "wrapper"
  makeFreeFn           :: FreeFn           -> IO (FunPtr FreeFn)


{-# INLINE fillBytes #-}
fillBytes               :: Ptr a -> Word8 -> Int -> IO ()
fillBytes dest char size = do
  _ <- memset dest (fromIntegral char) (fromIntegral size)
  return ()

{-# INLINE calloc #-}
calloc                  :: Storable a => IO (Ptr a)
calloc                   = doCalloc undefined
  where
    doCalloc       :: Storable b => b -> IO (Ptr b)
    doCalloc dummy  = _calloc (fromIntegral (sizeOf dummy))

foreign import ccall unsafe "stdlib.h calloc"  _calloc  ::          CSize -> IO (Ptr a)
foreign import ccall unsafe "string.h" memset  :: Ptr a -> CInt  -> CSize -> IO ()


-- int plugin_register_config (const char *name,
-- 		int (*callback) (const char *key, const char *val),
-- 		const char **keys, int keys_num);
