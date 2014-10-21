{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "include/collectd.h"
module Collectd.Collectd where
import Foreign.Ptr
#strict_import

{- typedef uint64_t cdtime_t; -}
#synonym_t cdtime_t , CULong
#globalvar hostname_g , CChar
#globalvar interval_g , CULong
#globalvar timeout_g , CInt
