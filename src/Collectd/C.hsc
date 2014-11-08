{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}

module Collectd.C where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Data.ByteString          (ByteString, packCString)

import Collectd.Internal
import Control.Monad (forM, mapM)
import Control.Applicative

#include <collectd.h>
#include <plugin.h>
#include <liboconfig/oconfig.h>
#include <custom.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

type CounterT  = CULong
type GaugeT    = CDouble
type DeriveT   = CLong
type AbsoluteT = CULong

-- |
-- | Data Types
-- |

data DataSet = DataSet
    { dstType :: !String
    , dstDs   :: ![DataSource]
    } deriving(Eq, Show)

data DataSource = DataSource
    { dsName :: !String
    , dsType :: !Int
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

data ValueList
type ValueListPtr    = Ptr ValueList

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
  customMem <- malloc
  _         <- memset (castPtr customMem) 0 #{size custom_t}
  _         <- poke customMem c

  return $ customMem

type CallbackFn      = CString -> CString -> CInt
type ConfigFn        = ConfigItemPtr -> CInt

type ConfigCallbackFn =
  ConfigItemPtr
  -> IO CInt

type WriteCallbackFn  =
  DataSetPtr
  -> ValueListPtr
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
  makeWriteCallbackFn  :: WriteCallbackFn -> IO (FunPtr WriteCallbackFn)

foreign import ccall safe "wrapper"
  makeConfigCallbackFn :: ConfigCallbackFn -> IO (FunPtr ConfigCallbackFn)

foreign import ccall safe "wrapper"
  makeFreeFn           :: FreeFn -> IO (FunPtr FreeFn)

foreign import ccall unsafe "stdlib.h memset"
  memset :: Ptr a -> CInt -> CSize -> IO ()


-- int plugin_register_config (const char *name,
-- 		int (*callback) (const char *key, const char *val),
-- 		const char **keys, int keys_num);
