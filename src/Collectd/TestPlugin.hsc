{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}

module Collectd.TestPlugin where

import Control.Concurrent

import qualified Collectd.C as C
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.C.String
import Foreign.Storable

registerWriteCallback :: C.WriteCallbackFn
registerWriteCallback dataSet userValues userData = do
  -- peek a >>= print
  userDataVals <- peek userData
  peek ((castPtr (C.udData userDataVals)) :: Ptr C.Custom) >>= print
  -- peek userData >>= print
  return 0

configCallback :: C.ConfigCallbackFn
configCallback config = do
  print "asd"
  peek config >>= print

  return 0

foreign export ccall module_register :: IO ()

-- (C.Custom "custom name" 100)

makeUserData :: C.Custom -> IO C.UserDataPtr
makeUserData c = do
  userDataMem <- malloc :: IO (Ptr C.UserData)
  custom      <- C.makeCustom c
  freeFn      <- C.makeFreeFn (\ptr -> free (castPtr ptr))
  poke userDataMem (C.UserData (castPtr custom) freeFn)
  return $ userDataMem

module_register :: IO ()
module_register = do
  registerWriteCallbackFn  <- C.makeWriteCallbackFn registerWriteCallback
  configCallbackFn         <- C.makeConfigCallbackFn configCallback
  userData                 <- makeUserData (C.Custom "custom name" 100)

  callbackName <- newCString "test_plugin"

  _     <- C.plugin_register_complex_config callbackName configCallbackFn
  _     <- C.plugin_register_write callbackName registerWriteCallbackFn userData

  return ()
