{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}

module Collectd.C where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Data.ByteString          (ByteString, packCString)

import Control.Monad (forM, mapM)
import Control.Applicative

#include <collectd.h>
#include <plugin.h>
#include <liboconfig/oconfig.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

data family Struct a

class Copy a where
    copy  :: Ptr (Struct a) -> IO a
    fromC :: Struct a -> IO a

type CounterT  = CULong
type GaugeT    = CDouble
type DeriveT   = CLong
type AbsoluteT = CULong

data DataSet = DataSet String [DataSource]
             deriving(Show)

type DataSetPtr      = Ptr DataSet



instance Storable DataSet where
  alignment _ = #{alignment data_set_t}
  sizeOf _    = #{size data_set_t}
  peek p = do
    typeS   <- peekCString $ #{ptr data_set_t, type} p
    dsNum  <- #{peek data_set_t, ds_num} p

    -- dss     <- forM [0..((mkInt dsNum)-1)] (\i -> peekElemOff (#{ptr data_set_t, ds} p) i)
    -- a     <- (peek $ (#{ptr data_set_t, ds} p) :: IO (Struct DataSource))
    a        <- (#{peek data_set_t, ds} p) :: IO (Struct DataSource)
    print $ a
    dss      <- peekArray (mkInt dsNum) ((#{ptr data_set_t, ds} p) :: DataSourcePtr)
    -- (packCString $ cdsName (dss !! 0)) >>= print
    -- (packCString $ cdsName (dss !! 1)) >>= print
    -- print dss
    -- zz <- mapM (\i -> packCString $ (cdsName i)) dss
    print $ (dss :: [(Struct DataSource)])

    -- print $ (a :: DataSource)
    -- e        <- mapM fromC dss
    return $ DataSet typeS []

  poke ptr (DataSet a ds) = undefined

data UserData
type UserDataPtr     = Ptr UserData

data ValueList
type ValueListPtr    = Ptr ValueList


data DataSource = DataSource
                  { dsName :: !ByteString
                  , dsType :: !Integer
                  , dsMin  :: !Double
                  , dsMax  :: !Double
                  } deriving (Eq, Show)

data instance Struct DataSource = CDataSource
                                  { cdsName   :: CString
                                  , cdsType   :: CInt
                                  , cdsMin    :: CDouble
                                  , cdsMax    :: CDouble
                                  } deriving(Show)

type DataSourcePtr   = Ptr (Struct DataSource)

instance Storable (Struct DataSource) where
  alignment _ = #{alignment data_source_t}
  sizeOf _    = #{size      data_source_t}

  peek p      = do
    a1 <- #{peek data_source_t, name} p
    print $ "pointer: " ++ (show #{ptr data_source_t, type} p)
    a2 <- #{peek data_source_t, type} p
    a3 <- #{peek data_source_t, min}  p
    a4 <- #{peek data_source_t, max}  p
    return $ CDataSource a1 a2 a3 a4


  poke p      = undefined

instance Copy DataSource where
  copy ptr = peek ptr >>= fromC

    fromC CDataSource{..} = do
        -- print cdsName
        -- aa <- peekCString cdsName
        -- print aa
        -- a <- packCString cdsName
        -- print a
        DataSource <$> packCString cdsName
                   <#> cdsType
                   <@> cdsMin
                   <@> cdsMax


-- instance Storable DataSource where
--   alignment _ = #{alignment data_source_t}
--   sizeOf _    = #{size data_source_t}
--   peek ptr = do
--     nameS  <- peekCString $ #{ptr data_source_t, name} ptr
--     t      <- #{peek data_source_t, type}  ptr
--     min_   <- #{peek data_source_t, min}   ptr
--     max_   <- #{peek data_source_t, max}   ptr
--     return (DataSource nameS t min_ max_ )
--   poke ptr (DataSource name t min_ max_) = undefined

data OConfigItem
type OConfigItemPtr  = Ptr OConfigItem

type CallbackFn     = CString -> CString -> CInt
type ConfigFn        = OConfigItemPtr -> CInt
type WriteCallbackFn = DataSetPtr -> ValueListPtr -> UserDataPtr -> IO CInt


foreign import ccall safe "collectd/plugin.h plugin_register_config"
  plugin_register_config :: CString -> FunPtr CallbackFn -> FunPtr CallbackFn -> Ptr CString -> IO CInt

foreign import ccall safe "collectd/plugin.h plugin_register_complex_config"
  plugin_register_complex_config :: CString -> FunPtr OConfigItemPtr -> IO CInt

foreign import ccall safe "collectd/plugin.h plugin_register_write"
  plugin_register_write :: CString -> FunPtr WriteCallbackFn -> UserDataPtr -> IO CInt

foreign import ccall safe "wrapper" mkWcb :: WriteCallbackFn -> IO (FunPtr WriteCallbackFn)


-- int plugin_register_config (const char *name,
-- 		int (*callback) (const char *key, const char *val),
-- 		const char **keys, int keys_num);




infixl 4 <%>, <#>, <@>, <!>

(<%>) :: (Integral a, Applicative f) => (Integer -> b) -> a -> f b
(<%>) a b = a <$> pure (toInteger b)

(<#>) :: (Integral a, Applicative f) => f (Integer -> b) -> a -> f b
(<#>) a b = a <*> pure (toInteger b)

(<@>) :: (Fractional a, Real c, Applicative f) => f (a -> b) -> c -> f b
(<@>) a b = a <*> pure (realToFrac b)

(<!>) :: Applicative f => f (a -> b) -> a -> f b
(<!>) a b = a <*> pure b







cSizeToInt :: CSize -> Int
cSizeToInt = fromIntegral
{-# INLINE cSizeToInt #-}

intToCSize :: Int -> CSize
intToCSize = fromIntegral
{-# INLINE intToCSize #-}

intToCInt :: Int -> CInt
intToCInt = fromIntegral
{-# INLINE intToCInt #-}

cIntToInt :: CInt -> Int
cIntToInt = fromIntegral
{-# INLINE cIntToInt #-}

mkInt :: CInt -> Int
mkInt = fromIntegral
