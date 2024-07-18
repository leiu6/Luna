#ifndef LUNA_MEMORY_H
#define LUNA_MEMORY_H

#include "defs.h"
#include "state.h"

/**
 * Standard allocation function for the Luna interpreter
 */
void *mem_reallocate(void *mem, size_t oldsize, size_t newsize, Luna_State *L);

#endif /* LUNA_MEMORY_H */
