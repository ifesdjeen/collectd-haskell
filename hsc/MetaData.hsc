{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "include/meta_data.h"
module Collectd.MetaData where
import Foreign.Ptr
#strict_import

import Collectd.Collectd
{- struct meta_data_s; -}
#opaque_t struct meta_data_s
{- typedef struct meta_data_s meta_data_t; -}
#opaque_t struct meta_data_s
#synonym_t meta_data_t , <struct meta_data_s>
#ccall meta_data_create , IO (Ptr <struct meta_data_s>)
#ccall meta_data_clone , Ptr <struct meta_data_s> -> IO (Ptr <struct meta_data_s>)
#ccall meta_data_destroy , Ptr <struct meta_data_s> -> IO ()
#ccall meta_data_exists , Ptr <struct meta_data_s> -> CString -> IO CInt
#ccall meta_data_type , Ptr <struct meta_data_s> -> CString -> IO CInt
#ccall meta_data_toc , Ptr <struct meta_data_s> -> Ptr (Ptr CString) -> IO CInt
#ccall meta_data_delete , Ptr <struct meta_data_s> -> CString -> IO CInt
#ccall meta_data_add_string , Ptr <struct meta_data_s> -> CString -> CString -> IO CInt
#ccall meta_data_add_signed_int , Ptr <struct meta_data_s> -> CString -> CLong -> IO CInt
#ccall meta_data_add_unsigned_int , Ptr <struct meta_data_s> -> CString -> CULong -> IO CInt
#ccall meta_data_add_double , Ptr <struct meta_data_s> -> CString -> CDouble -> IO CInt
#ccall meta_data_add_boolean , Ptr <struct meta_data_s> -> CString -> CInt -> IO CInt
#ccall meta_data_get_string , Ptr <struct meta_data_s> -> CString -> Ptr CString -> IO CInt
#ccall meta_data_get_signed_int , Ptr <struct meta_data_s> -> CString -> Ptr CLong -> IO CInt
#ccall meta_data_get_unsigned_int , Ptr <struct meta_data_s> -> CString -> Ptr CULong -> IO CInt
#ccall meta_data_get_double , Ptr <struct meta_data_s> -> CString -> Ptr CDouble -> IO CInt
#ccall meta_data_get_boolean , Ptr <struct meta_data_s> -> CString -> Ptr CInt -> IO CInt
