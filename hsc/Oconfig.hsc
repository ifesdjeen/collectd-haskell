{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "include/liboconfig/oconfig.h"
module Collectd.Oconfig where
import Foreign.Ptr
#strict_import

{- struct oconfig_value_s {
    union {
        char * string; double number; int boolean;
    } value;
    int type;
}; -}
#starttype struct oconfig_value_s
#field value ,
#field type , CInt
#stoptype
{- typedef struct oconfig_value_s oconfig_value_t; -}
#opaque_t struct oconfig_value_s
#synonym_t oconfig_value_t , <struct oconfig_value_s>
{- struct oconfig_item_s; -}
#opaque_t struct oconfig_item_s
{- typedef struct oconfig_item_s oconfig_item_t; -}
#opaque_t struct oconfig_item_s
#synonym_t oconfig_item_t , <struct oconfig_item_s>
{- struct oconfig_item_s {
    char * key;
    oconfig_value_t * values;
    int values_num;
    oconfig_item_t * parent;
    oconfig_item_t * children;
    int children_num;
}; -}
#starttype struct oconfig_item_s
#field key , CString
#field values , Ptr <struct oconfig_value_s>
#field values_num , CInt
#field parent , Ptr <struct oconfig_item_s>
#field children , Ptr <struct oconfig_item_s>
#field children_num , CInt
#stoptype
#ccall oconfig_free , Ptr <struct oconfig_item_s> -> IO ()
