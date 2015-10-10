/* FIXME remove, we only have that at the moment because in the current
   packaging setting OCaml libasmrun's library is compiled against
   newlib's headers. */

#include <errno.h>

void *_impure_ptr;
char *__ctype_ptr__;
int *__errno (void) { return &errno; }
