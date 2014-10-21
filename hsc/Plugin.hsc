{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "include/plugin.h"
module Collectd.Plugin where
import Foreign.Ptr
#strict_import

import Collectd.Collectd
import Collectd.Configfile
import Collectd.MetaData
import Collectd.UtilsTime
{- typedef unsigned long long counter_t; -}
#synonym_t counter_t , CULong
{- typedef double gauge_t; -}
#synonym_t gauge_t , CDouble
{- typedef int64_t derive_t; -}
#synonym_t derive_t , CLong
{- typedef uint64_t absolute_t; -}
#synonym_t absolute_t , CULong
{- union value_u {
    counter_t counter;
    gauge_t gauge;
    derive_t derive;
    absolute_t absolute;
}; -}
#starttype union value_u
#field counter , CULong
#field gauge , CDouble
#field derive , CLong
#field absolute , CULong
#stoptype
{- typedef union value_u value_t; -}
#opaque_t union value_u
#synonym_t value_t , <union value_u>
{- struct value_list_s {
    value_t * values;
    int values_len;
    cdtime_t time;
    cdtime_t interval;
    char host[64];
    char plugin[64];
    char plugin_instance[64];
    char type[64];
    char type_instance[64];
    meta_data_t * meta;
}; -}
#starttype struct value_list_s
#field values , Ptr <union value_u>
#field values_len , CInt
#field time , CULong
#field interval , CULong
#array_field host , CChar
#array_field plugin , CChar
#array_field plugin_instance , CChar
#array_field type , CChar
#array_field type_instance , CChar
#field meta , Ptr <struct meta_data_s>
#stoptype
{- typedef struct value_list_s value_list_t; -}
#opaque_t struct value_list_s
#synonym_t value_list_t , <struct value_list_s>
{- struct data_source_s {
    char name[64]; int type; double min; double max;
}; -}
#starttype struct data_source_s
#array_field name , CChar
#field type , CInt
#field min , CDouble
#field max , CDouble
#stoptype
{- typedef struct data_source_s data_source_t; -}
#opaque_t struct data_source_s
#synonym_t data_source_t , <struct data_source_s>
{- struct data_set_s {
    char type[64]; int ds_num; data_source_t * ds;
}; -}
#starttype struct data_set_s
#array_field type , CChar
#field ds_num , CInt
#field ds , Ptr <struct data_source_s>
#stoptype
{- typedef struct data_set_s data_set_t; -}
#opaque_t struct data_set_s
#synonym_t data_set_t , <struct data_set_s>
{- enum notification_meta_type_e {
    NM_TYPE_STRING,
    NM_TYPE_SIGNED_INT,
    NM_TYPE_UNSIGNED_INT,
    NM_TYPE_DOUBLE,
    NM_TYPE_BOOLEAN
}; -}
#integral_t enum notification_meta_type_e
#num NM_TYPE_STRING
#num NM_TYPE_SIGNED_INT
#num NM_TYPE_UNSIGNED_INT
#num NM_TYPE_DOUBLE
#num NM_TYPE_BOOLEAN
{- typedef struct notification_meta_s {
            char name[64];
            enum notification_meta_type_e type;
            union {
                const char * nm_string;
                int64_t nm_signed_int;
                uint64_t nm_unsigned_int;
                double nm_double;
                _Bool nm_boolean;
            } nm_value;
            struct notification_meta_s * next;
        } notification_meta_t; -}
#starttype struct notification_meta_s
#array_field name , CChar
#field type , <enum notification_meta_type_e>
#field nm_value ,
#field next , Ptr <struct notification_meta_s>
#stoptype
#synonym_t notification_meta_t , <struct notification_meta_s>
{- typedef struct notification_s {
            int severity;
            cdtime_t time;
            char message[256];
            char host[64];
            char plugin[64];
            char plugin_instance[64];
            char type[64];
            char type_instance[64];
            notification_meta_t * meta;
        } notification_t; -}
#starttype struct notification_s
#field severity , CInt
#field time , CULong
#array_field message , CChar
#array_field host , CChar
#array_field plugin , CChar
#array_field plugin_instance , CChar
#array_field type , CChar
#array_field type_instance , CChar
#field meta , Ptr <struct notification_meta_s>
#stoptype
#synonym_t notification_t , <struct notification_s>
{- struct user_data_s {
    void * data; void (* free_func)(void *);
}; -}
#starttype struct user_data_s
#field data , Ptr ()
#field free_func , FunPtr (Ptr () -> IO ())
#stoptype
{- typedef struct user_data_s user_data_t; -}
#opaque_t struct user_data_s
#synonym_t user_data_t , <struct user_data_s>
{- struct plugin_ctx_s {
    cdtime_t interval;
}; -}
#starttype struct plugin_ctx_s
#field interval , CULong
#stoptype
{- typedef struct plugin_ctx_s plugin_ctx_t; -}
#opaque_t struct plugin_ctx_s
#synonym_t plugin_ctx_t , <struct plugin_ctx_s>
#callback plugin_init_cb , IO CInt
#callback plugin_read_cb , Ptr <struct user_data_s> -> IO CInt
#callback plugin_write_cb , Ptr <struct data_set_s> -> Ptr <struct value_list_s> -> Ptr <struct user_data_s> -> IO CInt
#callback plugin_flush_cb , CULong -> CString -> Ptr <struct user_data_s> -> IO CInt
#callback plugin_missing_cb , Ptr <struct value_list_s> -> Ptr <struct user_data_s> -> IO CInt
#callback plugin_log_cb , CInt -> CString -> Ptr <struct user_data_s> -> IO ()
#callback plugin_shutdown_cb , IO CInt
#callback plugin_notification_cb , Ptr <struct notification_s> -> Ptr <struct user_data_s> -> IO CInt
#ccall plugin_set_dir , CString -> IO ()
#ccall plugin_load , CString -> CUInt -> IO CInt
#ccall plugin_init_all , IO ()
#ccall plugin_read_all , IO ()
#ccall plugin_read_all_once , IO CInt
#ccall plugin_shutdown_all , IO ()
#ccall plugin_write , CString -> Ptr <struct data_set_s> -> Ptr <struct value_list_s> -> IO CInt
#ccall plugin_flush , CString -> CULong -> CString -> IO CInt
-- DONE
#ccall plugin_register_config , CString -> FunPtr (CString -> CString -> CInt) -> Ptr CString -> CInt -> IO CInt
#ccall plugin_register_complex_config , CString -> FunPtr (Ptr <struct oconfig_item_s> -> CInt) -> IO CInt
#ccall plugin_register_init , CString -> <plugin_init_cb> -> IO CInt
#ccall plugin_register_read , CString -> FunPtr CInt -> IO CInt
#ccall plugin_register_complex_read , CString -> CString -> <plugin_read_cb> -> Ptr <struct timespec> -> Ptr <struct user_data_s> -> IO CInt
#ccall plugin_register_write , CString -> <plugin_write_cb> -> Ptr <struct user_data_s> -> IO CInt
#ccall plugin_register_flush , CString -> <plugin_flush_cb> -> Ptr <struct user_data_s> -> IO CInt
#ccall plugin_register_missing , CString -> <plugin_missing_cb> -> Ptr <struct user_data_s> -> IO CInt
#ccall plugin_register_shutdown , CString -> <plugin_shutdown_cb> -> IO CInt
#ccall plugin_register_data_set , Ptr <struct data_set_s> -> IO CInt
#ccall plugin_register_log , CString -> <plugin_log_cb> -> Ptr <struct user_data_s> -> IO CInt
#ccall plugin_register_notification , CString -> <plugin_notification_cb> -> Ptr <struct user_data_s> -> IO CInt
#ccall plugin_unregister_config , CString -> IO CInt
#ccall plugin_unregister_complex_config , CString -> IO CInt
#ccall plugin_unregister_init , CString -> IO CInt
#ccall plugin_unregister_read , CString -> IO CInt
#ccall plugin_unregister_read_group , CString -> IO CInt
#ccall plugin_unregister_write , CString -> IO CInt
#ccall plugin_unregister_flush , CString -> IO CInt
#ccall plugin_unregister_missing , CString -> IO CInt
#ccall plugin_unregister_shutdown , CString -> IO CInt
#ccall plugin_unregister_data_set , CString -> IO CInt
#ccall plugin_unregister_log , CString -> IO CInt
#ccall plugin_unregister_notification , CString -> IO CInt
#ccall plugin_dispatch_values , Ptr <struct value_list_s> -> IO CInt
#ccall plugin_dispatch_missing , Ptr <struct value_list_s> -> IO CInt
#ccall plugin_dispatch_notification , Ptr <struct notification_s> -> IO CInt
#ccall plugin_log , CInt -> CString -> IO ()
#ccall parse_log_severity , CString -> IO CInt
#ccall parse_notif_severity , CString -> IO CInt
#ccall plugin_get_ds , CString -> IO (Ptr <struct data_set_s>)
#ccall plugin_notification_meta_add_string , Ptr <struct notification_s> -> CString -> CString -> IO CInt
#ccall plugin_notification_meta_add_signed_int , Ptr <struct notification_s> -> CString -> CLong -> IO CInt
#ccall plugin_notification_meta_add_unsigned_int , Ptr <struct notification_s> -> CString -> CULong -> IO CInt
#ccall plugin_notification_meta_add_double , Ptr <struct notification_s> -> CString -> CDouble -> IO CInt
#ccall plugin_notification_meta_add_boolean , Ptr <struct notification_s> -> CString -> CInt -> IO CInt
#ccall plugin_notification_meta_copy , Ptr <struct notification_s> -> Ptr <struct notification_s> -> IO CInt
#ccall plugin_notification_meta_free , Ptr <struct notification_meta_s> -> IO CInt
#ccall plugin_init_ctx , IO ()
#ccall plugin_get_ctx , IO (<struct plugin_ctx_s>)
#ccall plugin_set_ctx , <struct plugin_ctx_s> -> IO (<struct plugin_ctx_s>)
#ccall plugin_get_interval , IO CULong
#ccall plugin_thread_create , Ptr <struct _opaque_pthread_t> -> Ptr <struct _opaque_pthread_attr_t> -> FunPtr (Ptr () -> Ptr ()) -> Ptr () -> IO CInt
