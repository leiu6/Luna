#ifndef LUNA_VM_H
#define LUNA_VM_H

#include "state.h"
#include "object.h"
#include <setjmp.h>

/**
 * Represents an entry in the call stack.
 */
typedef struct {
    Function *function;
    u8 *ip;
    Value *slots;
} CallFrame;

#define MAX_RECURSION (0xFF)
#define STACK_SIZE (0xFF * 0xFF)

/**
 * Holds the state for the actual virtual machine.
 */
typedef struct {
    Luna_State *L; /* Interpreter instance */
    Value stack[STACK_SIZE]; /* Eval stack */
    Value *stacktop;
    CallFrame frames[MAX_RECURSION]; /* Call stack */
    CallFrame *frametop;
    Table *globals; /* Global variables */
    jmp_buf error; /* Jump point for VM error */
} VM;

/**
 * Initialize a virtual machine.
 */
void vm_init(Luna_State *L, VM *M);

/**
 * Execute a function object in the virtual machine.
 */
int vm_exec(VM *M, Function *f);

void vm_stack_push(VM *M, Value v);
Value vm_stack_pop(VM *M);
Value vm_stack_peek(VM *M, int offset);

#endif /* LUNA_VM_H */
