{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings #-}

module Collectd.ContinuumTest where

import qualified Collectd.C            as C
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.C.String
import           Foreign.Storable
import           Continuum.Client.Base as Continuum
--import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B
import           Collectd.Types
import qualified Data.Map as Map

toDbValue :: CollectdValue -> Continuum.DbValue
toDbValue (GaugeT v) = Continuum.DbInt (ceiling v)
toDbValue _ = undefined

write :: Continuum.ContinuumClient -> C.WriteCallbackFn
write client dataSet valueList userData = do
  print "writing"
  values    <- C.unpackValueList dataSet valueList
  let sendData ValueList{..} =
        mapM (\(_, v) ->
               (Continuum.executeQuery client
                     (Continuum.Insert
                      "memory"
                      (Continuum.makeRecord
                       (fromIntegral vTime)
                       [("host",    DbString (B.pack vHost))
                       ,("subtype", DbString (B.pack vTypeInstance))
                       ,("value",   toDbValue v)])))
             )  (Map.toList vValues)
  _         <- sendData values

  return 0


makeUserData :: C.Custom -> IO C.UserDataPtr
makeUserData c = do
  userDataMem <- malloc :: IO (Ptr C.UserData)
  custom      <- C.makeCustom c
  freeFn      <- C.makeFreeFn (\ptr -> free (castPtr ptr))
  poke userDataMem (C.UserData (castPtr custom) freeFn)
  return $ userDataMem



memorySchema =
  Continuum.makeSchema [ ("host",    Continuum.DbtString)
                       , ("value",   Continuum.DbtInt)
                       , ("subtype", Continuum.DbtString) ]

foreign export ccall module_register :: IO ()

module_register :: IO ()
module_register = do
  client <- Continuum.connect "127.0.0.1" "5566"
  -- _      <- Continuum.executeQuery client (Continuum.CreateDb "memory" memorySchema)

  userData <- makeUserData (C.Custom "custom name" 100)
  callbackName <- newCString "test_plugin"

  registerWriteCallbackFn  <- C.makeWriteCallbackFn (write client)
  _     <- C.plugin_register_write callbackName registerWriteCallbackFn userData

  --

  -- _     <- C.plugin_register_complex_config callbackName configCallbackFn


  return ()
