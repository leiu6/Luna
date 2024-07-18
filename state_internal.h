#ifndef LUNA_STATE_INTERNAL_H
#define LUNA_STATE_INTERNAL_H

#include "state.h"

#include "defs.h"
#include "setjmp.h"
#include "vm.h"
#include <stdio.h>

struct Luna_State {
    void *(*reallocate)(void *, size_t, size_t, Luna_State *); /* Allocator */
    FILE *out; /* Output file */
    FILE *error; /* Error output file */
    FILE *input; /* Input file */
    jmp_buf oom; /* Out of memory jump point */
    VM vm; /* Virtual machine instance */
};

/* Memory allocation macros */

#define ALLOCATE(L, size) \
    ((L)->reallocate(NULL, 0, size, (L)))

#define DEALLOCATE(L, ptr, size) \
    ((L)->reallocate(ptr, size, 0, (L)))

#define RESIZE(L, ptr, oldsize, newsize) \
    ((L)->reallocate(ptr, oldsize, newsize, (L)))

#define ARRAY_ALLOCATE(L, T, count) \
    ((L)->reallocate(NULL, 0, sizeof(T) * count, (L)))

#define ARRAY_GROW(L, T, ptr, oldcount, newcount)			\
    ((L)->reallocate(ptr, sizeof(T) * oldcount, sizeof(T) * newcount, (L)))

#endif /* LUNA_STATE_INTERNAL_H */
