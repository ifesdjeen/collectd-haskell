{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}

module Collectd.TestPlugin where

import Control.Concurrent

import qualified Collectd.C as C
import Foreign.Ptr
import Foreign.C.String
import Foreign.Storable

wcb :: C.WriteCallbackFn
wcb a b c = do
  print "YOYO"
  peek a >>= print
  print "YOYO22"
  -- print a
  -- print b
  -- print c
  return 1

foreign export ccall module_register :: IO ()
module_register :: IO ()
module_register = do
  print rtsSupportsBoundThreads


  print "FIRST STEP12312"
  wcb'  <- C.mkWcb wcb
  name' <- newCString "test_plugin"
  _     <- C.plugin_register_write name' wcb' nullPtr

  return ()
