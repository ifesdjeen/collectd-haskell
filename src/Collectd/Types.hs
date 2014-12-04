module Collectd.Types where

import qualified Data.Map as Map
import           Data.Word                ( Word64(..))

data CollectdValue =
     CounterT    Integer
     | GaugeT    Double
     | DeriveT   Integer
     | AbsoluteT Integer
     deriving(Eq, Show)

data ValueList = ValueList
    { vPlugin         :: !String
    , vPluginInstance :: !String
    , vType           :: !String
    , vTypeInstance   :: !String
    , vHost           :: !String
    , vInterval       :: !Word64
    , vTime           :: !Word64
    , vValues         :: Map.Map String CollectdValue
    } deriving(Eq, Show)
