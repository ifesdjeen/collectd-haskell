{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "include/configfile.h"
module Collectd.Configfile where
import Foreign.Ptr
#strict_import

import Collectd.Collectd
import Collectd.UtilsTime
import Collectd.Liboconfig.Oconfig
#ccall cf_unregister , CString -> IO ()
#ccall cf_unregister_complex , CString -> IO ()
#ccall cf_register , CString -> FunPtr (CString -> CString -> CInt) -> Ptr CString -> CInt -> IO ()
#ccall cf_register_complex , CString -> FunPtr (Ptr <struct oconfig_item_s> -> CInt) -> IO CInt
#ccall cf_read , CString -> IO CInt
#ccall global_option_set , CString -> CString -> IO CInt
#ccall global_option_get , CString -> IO CString
#ccall global_option_get_long , CString -> CLong -> IO CLong
#ccall global_option_get_long_in_range , CString -> CLong -> CLong -> CLong -> IO CLong
#ccall cf_get_default_interval , IO CULong
#ccall cf_util_get_string , Ptr <struct oconfig_item_s> -> Ptr CString -> IO CInt
#ccall cf_util_get_string_buffer , Ptr <struct oconfig_item_s> -> CString -> CSize -> IO CInt
#ccall cf_util_get_int , Ptr <struct oconfig_item_s> -> Ptr CInt -> IO CInt
#ccall cf_util_get_double , Ptr <struct oconfig_item_s> -> Ptr CDouble -> IO CInt
#ccall cf_util_get_boolean , Ptr <struct oconfig_item_s> -> Ptr CInt -> IO CInt
#ccall cf_util_get_flag , Ptr <struct oconfig_item_s> -> Ptr CUInt -> CUInt -> IO CInt
#ccall cf_util_get_port_number , Ptr <struct oconfig_item_s> -> IO CInt
#ccall cf_util_get_service , Ptr <struct oconfig_item_s> -> Ptr CString -> IO CInt
#ccall cf_util_get_cdtime , Ptr <struct oconfig_item_s> -> Ptr CULong -> IO CInt
