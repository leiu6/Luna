#include "vm.h"

#include "state_internal.h"
#include "opcodes.h"
#include <stdio.h>
#include <stdarg.h>

/* Forward */
NORETURN static void error(VM *M, const char *format, ...);

/**
 * Pushes a call frame onto the stack for the given function
 */
static void push_frame(VM *M, Function *f);

/**
 * Pops a call frame off of the stack.
 */
static void pop_frame(VM *M);

void vm_init(Luna_State *L, VM *M) {
    M->L = L;
    M->stacktop = M->stack;
    M->frametop = M->frames;
    M->globals = NULL; /* TODO: implement tables */
}

/**
 * Reads a byte from the current function and advances the ip.
 */
#define READ_BYTE(M) \
    (*((M)->frametop[-1].ip++))

/**
 * Reads two bytes from the current function and advances the ip.
 */
#define READ_WORD(M) \
    (((M->frametop[-1].ip[0]) << 8) + (M->frametop[-1].ip[1])); \
    (M->frametop[-1].ip += 2);

/**
 * Gets the constant from the current function at the given index.
 */
#define READ_CONSTANT(M, index) \
    (M->frametop[-1].function->constants[index])

int vm_exec(VM *M, Function *f) {
    LUNA_ASSERT(f != NULL);
    if (setjmp(M->error) != 0) { /* Did runtime error happen? */
	return 1;
    }

    vm_stack_push(M, value_function(f)); /* Push root function onto stack */
    push_frame(M, f); /* Setup call stack */
    
    for (;;) {
	Opcode op = READ_BYTE(M);

	switch (op) {
	case OP_PUSH: {
	    u16 index = READ_WORD(M);
	    Value value = READ_CONSTANT(M, index);
	    vm_stack_push(M, value);
	} break;

	case OP_POP: {
	    (void)vm_stack_pop(M);
	} break;

	case OP_RETURN: {
	    u16 arity = READ_WORD(M);
	    /* TODO: implement more */
	    return 0;
	} break;
	}
    }
    return 0;
}

void vm_stack_push(VM *M, Value v) {
    if (M->stacktop > M->stack + STACK_SIZE) {
	error(M, "stack overflow!");
    }
    *M->stacktop++ = v;
}

Value vm_stack_pop(VM *M) {
    if (M->stacktop == M->stack) {
	error(M, "stack underflow!");
    }
    return *(--M->stacktop);
}

/* Defintions */
void error(VM *M, const char *format, ...) {
    va_list args;
    va_start(args, format);
    fprintf(M->L->out, "[!] ");
    vfprintf(M->L->out, format, args);
    fprintf(M->L->out, "\n");
    longjmp(M->error, 1);
}

void push_frame(VM *M, Function *f) {
    CallFrame frame = {0};
    frame.function = f;
    frame.ip = f->code;
    LUNA_ASSERT(f->arity >= 0);
    frame.slots = M->stacktop - f->arity - 1;
    if (M->frametop > M->frames + MAX_RECURSION)
	error(M, "max recursion reached");
    *M->frametop++ = frame;
}
