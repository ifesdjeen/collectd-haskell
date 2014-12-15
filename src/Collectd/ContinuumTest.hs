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
import           Control.Monad          ( when, forM_ )
import           Data.Maybe             ( fromMaybe )

import qualified Collectd.C            as C
import qualified Data.ByteString.Char8 as B
import qualified Data.Map              as Map

toDbValue :: CollectdValue -> Continuum.DbValue
toDbValue (GaugeT     v) = Continuum.DbDouble v
toDbValue (CounterT   v) = Continuum.DbLong (fromIntegral v)
toDbValue (DeriveT    v) = Continuum.DbLong (fromIntegral v)
toDbValue (AbsoluteT  v) = Continuum.DbLong (fromIntegral v)
toDbValue _ = undefined

write :: Continuum.ContinuumClient
         -> TVar (Map.Map (DbValue,B.ByteString,Integer) [(FieldName, DbValue)])
         -> TVar (Maybe Integer)
         -> C.WriteCallbackFn

write client state flushCounter dataSet valueList userData = do
  values    <- C.unpackValueList dataSet valueList

  let timestamp  = fromIntegral $ vTime values

  valuesToFlush <- atomically $ do
    maybeOldCounter <- readTVar flushCounter
    oldValues       <- readTVar state
    _               <- modifyTVar flushCounter (swapCounter timestamp)

    case maybeOldCounter of
      (Just oldTimestamp) | timestamp > oldTimestamp -> do
        _ <- writeTVar state (Map.empty)
        _ <- enqueue state values
        return (Just oldValues)
      _                                              -> do
        _ <- enqueue state values
        return Nothing

  print $ valuesToFlush
  -- _ <- flush client valuesToFlush

  return 0

  where enqueue state ValueList{..}    =
          modifyTVar state (Map.alter
                            (append $ map (\x -> (B.pack vTypeInstance, x)) $ map toDbValue (Map.elems vValues))
                            (DbString $ B.pack vHost,
                             B.pack vPluginInstance,
                             fromIntegral vTime))
        append x Nothing          = Just x
        append x (Just old)       = Just (x ++ old)

        swapCounter newv Nothing  = Just newv
        swapCounter newv (Just _) = Just newv

flush :: Continuum.ContinuumClient
         -> Maybe (Map.Map (DbValue,B.ByteString,Integer) [(FieldName, DbValue)])
         -> IO ()
flush client (Just values) = do
  _ <- forM_ (Map.toList values) $ \((host, collection, timestamp), values) ->
    Continuum.sendRequest client $
    Continuum.Insert collection $
    Continuum.makeRecord timestamp (("host", host) : values)

  return ()
flush _ _ = return ()

--   requests  <- atomRead state
--   _         <- atomReset state []
--   _         <- atomReset flushCounter 0
--   _         <- mapM (\i -> do
--                         print i
--                         Continuum.sendRequest client i) requests
-- return ()


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
                       , ("actual_used",   Continuum.DbtLong)
                       , ("actual_free",   Continuum.DbtLong)
                       , ("used",   Continuum.DbtLong)
                       , ("free",   Continuum.DbtLong)
                       , ("total",   Continuum.DbtLong)
                       , ("ram",   Continuum.DbtLong) ]

memoryPercentSchema =
  Continuum.makeSchema [ ("host",    Continuum.DbtString)
                       , ("used_percent",   Continuum.DbtDouble)
                       , ("free_percent",   Continuum.DbtDouble)]

cpuSchema =
  Continuum.makeSchema [ ("host",    Continuum.DbtString)
                       , ("combined",   Continuum.DbtDouble)
                       , ("stolen",   Continuum.DbtDouble)
                       , ("soft_irq",   Continuum.DbtDouble)
                       , ("irq",   Continuum.DbtDouble)
                       , ("idle",   Continuum.DbtDouble)
                       , ("wait",   Continuum.DbtDouble)
                       , ("user",   Continuum.DbtDouble)
                       , ("sys",   Continuum.DbtDouble)
                       , ("nice",   Continuum.DbtDouble)]

networkSchema =
  Continuum.makeSchema [ ("host",    Continuum.DbtString)
                       , ("all_outbound_total",   Continuum.DbtLong)
                       , ("all_inbound_total",   Continuum.DbtLong)
                       , ("tcp_outbound_total",   Continuum.DbtLong)
                       , ("tcp_inbound_total",   Continuum.DbtLong) ]

tcpSchema =
  Continuum.makeSchema [ ("host",    Continuum.DbtString)
                       , ("out_rsts",   Continuum.DbtLong)
                       , ("in_errs",   Continuum.DbtLong)
                       , ("retrans_segs",   Continuum.DbtLong)
                       , ("out_segs",   Continuum.DbtLong)
                       , ("in_segs",   Continuum.DbtLong)
                       , ("curr_estab",   Continuum.DbtLong)
                       , ("estab_resets",   Continuum.DbtLong)
                       , ("attempt_fails",   Continuum.DbtLong)
                       , ("passive_opens",   Continuum.DbtLong)
                       , ("active_opens",   Continuum.DbtLong) ]

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

  flushState   <- atomically $ newTVar Map.empty
  flushCounter <- atomically $ newTVar Nothing

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
