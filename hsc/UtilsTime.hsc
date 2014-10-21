{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "include/utils_time.h"
module Collectd.UtilsTime where
import Foreign.Ptr
#strict_import

import Collectd.Collectd
#ccall cdtime , IO CULong
#ccall cdtime_to_iso8601 , CString -> CSize -> CULong -> IO CSize
