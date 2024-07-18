#include "memory.h"

#include "state_internal.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

void *mem_reallocate(void *mem, size_t oldsize, size_t newsize, Luna_State *L) {
    if (oldsize >= 0 && newsize > 0) {
	void *temp = realloc(mem, newsize);
	if (!temp) {
	    longjmp(L->oom, 1);
	}
	return temp;
    } else if (newsize == 0 && oldsize > 0) {
	free(mem);
	return NULL;
    } else {
	assert(0 && "invalid usage of mem_reallocate()");
    }
}
