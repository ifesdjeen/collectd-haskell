{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}
module Collectd.C where

import Foreign
import Foreign.C.Types
import Foreign.C.String

#include <collectd.h>
#include <plugin.h>
#include <liboconfig/oconfig.h>

type CounterT = CULong
type GaugeT = CDouble
type DeriveT = CLong
type AbsoluteT = CULong

data DataSet = DataSet CString CInt
             deriving(Show)

type DataSetPtr      = Ptr DataSet

instance Storable DataSet where
-- alignment _ = #{alignment data_set_t}
  sizeOf _    = #{size data_set_t}
  peek ptr = do
    a <- #{peek data_set_t, type} ptr
    b <- #{peek data_set_t, ds_num} ptr
    return (DataSet a b)
  poke ptr (DataSet a b) = do
    #{poke data_set_t, type} ptr a
    #{poke data_set_t, ds_num} ptr b


data UserData
type UserDataPtr     = Ptr UserData

data ValueList
type ValueListPtr    = Ptr ValueList

data DataSource
type DataSourcePtr   = Ptr DataSource

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

foreign export ccall initializeHaskell :: IO ()
initializeHaskell :: IO ()
initializeHaskell = do
  print "INITIALIZING"
  return ()



-- int plugin_register_config (const char *name,
-- 		int (*callback) (const char *key, const char *val),
-- 		const char **keys, int keys_num);
