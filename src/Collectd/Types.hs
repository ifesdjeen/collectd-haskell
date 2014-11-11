module Collectd.Types where

import qualified Data.Map as Map

data CollectdValue =
     CounterT    Int
     | GaugeT    Double
     | DeriveT   Int
     | AbsoluteT Int
     deriving(Eq, Show)

data ValueList = ValueList
    { vPlugin         :: !String
    , vPluginInstance :: !String
    , vType           :: !String
    , vTypeInstance   :: !String
    , vHost           :: !String
    , vInterval       :: !Int
    , vTime           :: !Int
    , vValues         :: Map.Map String CollectdValue
    } deriving(Eq, Show)
