{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}

module Collectd.C where

import Foreign
import Foreign.C.Types
import Foreign.C.String

-- #include <collectd.h>
-- #include <plugin.h>
-- #include <liboconfig/oconfig.h>

-- type CounterT = CULong
-- type GaugeT = CDouble
-- type DeriveT = CLong
-- type AbsoluteT = CULong

-- data DataSet
-- type DataSetPtr      = Ptr DataSet

-- data ValueList
-- type ValueListPtr    = Ptr ValueList

-- data DataSource
-- type DataSourcePtr   = Ptr DataSource

-- data OConfigItem
-- type OConfigItemPtr  = Ptr OConfigItem

-- type CallbackFn    = CString -> CString -> CInt
-- type ConfigFn      = OConfigItemPtr -> CInt

-- foreign import ccall safe "collectd/plugin.h plugin_register_config"
--   plugin_register_config :: CString -> FunPtr CallbackFn -> FunPtr CallbackFn -> Ptr CString -> IO CInt

-- foreign import ccall safe "collectd/plugin.h plugin_register_complex_config"
--   plugin_register_complex_config :: CString -> FunPtr OConfigItemPtr -> IO CInt

data OConfigItem
type OConfigItemPtr  = Ptr OConfigItem


foreign export ccall initializeHaskell :: IO ()
initializeHaskell :: IO ()
initializeHaskell = do
  print "INITIALIZING"
  return ()



-- int plugin_register_config (const char *name,
-- 		int (*callback) (const char *key, const char *val),
-- 		const char **keys, int keys_num);
