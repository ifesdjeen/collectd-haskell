{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}

module Collectd.TestPlugin where

import Control.Concurrent

import qualified Collectd.C as C
import Foreign.Ptr
import Foreign.C.String
import Foreign.Storable

writeCallback :: C.WriteCallbackFn
writeCallback a b c = do
  -- peek a >>= print

  return 0

configCallback :: C.ConfigCallbackFn
configCallback config = do
  print "asd"
  peek config >>= print

  return 0

foreign export ccall module_register :: IO ()

module_register :: IO ()
module_register = do
  writeCallbackFn   <- C.makeWriteCallbackFn writeCallback
  configCallbackFn  <- C.makeConfigCallbackFn configCallback

  callbackName <- newCString "test_plugin"

  _     <- C.plugin_register_complex_config callbackName configCallbackFn
  _     <- C.plugin_register_write callbackName writeCallbackFn nullPtr


  return ()
