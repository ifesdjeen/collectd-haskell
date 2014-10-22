#include "stdio.h"
/* #include "HsFFI.h" */
#include "Rts.h"
#include "C_stub.h"

void
module_register (void) {
        /* char *arr[1] = { "-N2" }; */
        /* char **arr_cast = arr; */
        /* int argc = 1; */

        // hs_init (i, myArray);

        /* printf("%s", "trying to turn on rts"); */

        /* hs_init(&argc, &arr_cast); */
        hs_init((void*)0, (void*)0);
        /* initializeHaskell(); */
}
