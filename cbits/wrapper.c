#include "stdio.h"
#include "HsFFI.h"
#include "C_stub.h"

void
module_register (void) {
        hs_init ((void*)0, (void*)0);
        initializeHaskell();
}
