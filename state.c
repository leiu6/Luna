#include "state_internal.h"

#include "memory.h"
#include "parser.h"
#include <stdlib.h>

Luna_State *luna_state_new(void) {
    Luna_State *L = calloc(1, sizeof(Luna_State));
    if (!L) {
	return NULL;
    }
    L->reallocate = mem_reallocate;
    L->out = stdout;
    L->error = stderr;
    L->input = stdin;
    vm_init(L, &L->vm); /* TODO: handle error if VM cannot create table */
    return L;
}

void luna_state_delete(Luna_State *L) {
    free(L);
}

int luna_execute_string(Luna_State *L, const char *src) {
    ParseState parser = {0};
    Function *func = NULL;

    /* Out of memory error point */
    if (setjmp(L->oom) != 0) {
	return LUNA_OOM_ERROR;
    }
    
    parser_init(L, &parser, src);
    func = parser_parse(&parser);
    if (!func) {
	return LUNA_COMPILE_ERROR;
    }
    int ret = vm_exec(&L->vm, func);
    if (ret != 0) {
	return LUNA_RUNTIME_ERROR;
    }
    return 0;
}
