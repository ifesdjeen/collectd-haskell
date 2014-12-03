{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings #-}

module Collectd.ContinuumTest where

import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.C.String
import           Foreign.Storable
import           Continuum.Client.Base as Continuum
import           Control.Concurrent.STM

--import qualified Data.ByteString       as B

import           Collectd.Types
import           Control.Monad         ( when )
import qualified Collectd.C            as C
import qualified Data.ByteString.Char8 as B
import qualified Data.Map              as Map

toDbValue :: CollectdValue -> Continuum.DbValue
toDbValue (GaugeT     v) = Continuum.DbDouble v
toDbValue (CounterT   v) = Continuum.DbLong (fromIntegral v)
toDbValue (DeriveT    v) = Continuum.DbLong (fromIntegral v)
toDbValue (AbsoluteT  v) = Continuum.DbLong (fromIntegral v)
toDbValue _ = undefined

write :: Continuum.ContinuumClient -> TVar [Continuum.Request] -> TVar Integer -> C.WriteCallbackFn
write client state flushCounter dataSet valueList userData = do
  values    <- C.unpackValueList dataSet valueList

  let sendData ValueList{..} =
        mapM (\(_, v) ->
               (swap state
                (\oldSt ->
                  (Continuum.Insert
                   (B.pack vPluginInstance)
                   (Continuum.makeRecord
                    (fromIntegral vTime)
                    [("host",    DbString (B.pack vHost))
                    ,("subtype", DbString (B.pack vTypeInstance))
                    ,("value",   toDbValue v)]))
                  : oldSt
                )))  (Map.toList vValues)
  _         <- sendData values

  _ <- swap flushCounter (+ 1)
  currentCounter <- atomRead flushCounter

  when (currentCounter > 50) (flush client state flushCounter)

  -- let sendData ValueList{..} =
  --       mapM (\(_, v) ->
  --              (Continuum.sendRequest client
  --               (Continuum.Insert
  --                "memory"
  --                (Continuum.makeRecord
  --                 (fromIntegral vTime)
  --                 [("host",    DbString (B.pack vHost))
  --                 ,("subtype", DbString (B.pack vTypeInstance))
  --                 ,("value",   toDbValue v)])))
  --            )  (Map.toList vValues)
  -- _         <- sendData values

  return 0


flush :: Continuum.ContinuumClient -> TVar [Continuum.Request] -> TVar Integer -> IO ()
flush client state flushCounter = do
  requests  <- atomRead state
  _         <- atomReset state []
  _         <- atomReset flushCounter 0
  _         <- mapM (\i -> do
                        print i
                        Continuum.sendRequest client i) requests


  return ()


makeUserData :: C.Custom -> IO C.UserDataPtr
makeUserData c = do
  userDataMem <- malloc :: IO (Ptr C.UserData)
  custom      <- C.makeCustom c
  freeFn      <- C.makeFreeFn (\ptr -> free (castPtr ptr))
  poke userDataMem (C.UserData (castPtr custom) freeFn)
  return $ userDataMem


configCallback :: C.ConfigCallbackFn
configCallback config = do
  peek config >>= print
  return 0

memorySchema =
  Continuum.makeSchema [ ("host",    Continuum.DbtString)
                       , ("value",   Continuum.DbtLong)
                       , ("subtype", Continuum.DbtString) ]

memoryPercentSchema =
  Continuum.makeSchema [ ("host",    Continuum.DbtString)
                       , ("value",   Continuum.DbtDouble)
                       , ("subtype", Continuum.DbtString) ]

cpuSchema =
  Continuum.makeSchema [ ("host",    Continuum.DbtString)
                       , ("value",   Continuum.DbtDouble)
                       , ("subtype", Continuum.DbtString) ]

networkSchema =
  Continuum.makeSchema [ ("host",    Continuum.DbtString)
                       , ("value",   Continuum.DbtLong)
                       , ("subtype", Continuum.DbtString) ]

tcpSchema =
  Continuum.makeSchema [ ("host",    Continuum.DbtString)
                       , ("value",   Continuum.DbtLong)
                       , ("subtype", Continuum.DbtString) ]

foreign export ccall module_register :: IO ()

module_register :: IO ()
module_register = do
  client <- Continuum.connect "127.0.0.1" "5566"
  _      <- Continuum.sendRequest client (Continuum.CreateDb "system.cpu" cpuSchema)
  _      <- Continuum.sendRequest client (Continuum.CreateDb "system.mem" memorySchema)
  _      <- Continuum.sendRequest client (Continuum.CreateDb "system.mem.percent" memorySchema)
  _      <- Continuum.sendRequest client (Continuum.CreateDb "system.net" networkSchema)
  _      <- Continuum.sendRequest client (Continuum.CreateDb "system.tcp" tcpSchema)

  userData <- makeUserData (C.Custom "custom name" 100)

  flushState   <- atomically $ newTVar []
  flushCounter <- atomically $ newTVar 0

  writeFn  <- C.makeWriteCallbackFn (write client flushState flushCounter)
  configFn <- C.makeConfigCallbackFn configCallback

  callbackName <- newCString "test_plugin"
  _     <- C.plugin_register_write callbackName writeFn userData

  _     <- C.plugin_register_complex_config callbackName configFn


  return ()



atomRead :: TVar a -> IO a
atomRead = atomically . readTVar

swap :: TVar b -> (b -> b) -> IO ()
swap x fn = atomically $ readTVar x >>= writeTVar x . fn

atomReset :: TVar b -> b -> IO ()
atomReset x newv = atomically $ writeTVar x newv
