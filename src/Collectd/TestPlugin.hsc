{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}

module Collectd.TestPlugin where

foreign export ccall module_register :: IO ()
module_register :: IO ()
module_register = do
  print "THIS IS MY TEST PLUGIN SHOULD JUST WERRKZ"
  return ()
