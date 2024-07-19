#include "vm.h"

#include "state_internal.h"
#include "opcodes.h"
#include <stdio.h>
#include <stdarg.h>
#include <string.h>

/* Forward */
NORETURN static void error(VM *M, const char *format, ...);

/**
 * Pushes a call frame onto the stack for the given function
 */
static void push_frame(VM *M, Function *f);

/**
 * Pops a call frame off of the stack.
 */
static void pop_frame(VM *M) __attribute__((unused));

void vm_init(Luna_State *L, VM *M) {
    M->L = L;
    M->stacktop = M->stack;
    M->frametop = M->frames;
    M->globals = table_alloc(L);
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

#define STACK_AT(M, index) \
    (M->stacktop[index])

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
	case OP_ADD: {
	    Value left, right;

	    right = vm_stack_pop(M);
	    left = vm_stack_pop(M);
	    if (value_is_int(left) && value_is_int(right)) {
		int l, r;
		
		l = value_as_int(left);
		r = value_as_int(right);
		vm_stack_push(M, value_int(l + r));
	    } else if (value_is_number(left) && value_is_number(right)) {
		double l, r;

		l = value_as_number(left);
		r = value_as_number(right);
		vm_stack_push(M, value_double(l + r));
	    } else if (value_is_string(left) || value_is_string(right)) {
		String *l, *r;

		l = value_as_string_repr(M->L, left);
		r = value_as_string_repr(M->L, right);
		vm_stack_push(M, value_string(string_alloc_concat(
					       M->L,
					       l->str,
					       l->length,
					       r->str,
					       r->length)));
	    } else {
		error(M, "operands not supported for '+'");
	    }
	} break;

	case OP_SUBTRACT: {
	    Value left, right;

	    right = vm_stack_pop(M);
	    left = vm_stack_pop(M);
	    if (value_is_int(left) && value_is_int(right)) {
		int l, r;
		
		l = value_as_int(left);
		r = value_as_int(right);
		vm_stack_push(M, value_int(l - r));
	    } else if (value_is_number(left) && value_is_number(right)) {
		double l, r;

		l = value_as_number(left);
		r = value_as_number(right);
		vm_stack_push(M, value_double(l - r));
	    } else {
		error(M, "operands not supported for '-'");
	    }
	} break;

	case OP_MULTIPLY: {
	    Value left, right;

	    right = vm_stack_pop(M);
	    left = vm_stack_pop(M);
	    if (value_is_int(left) && value_is_int(right)) {
		int l, r;
		
		l = value_as_int(left);
		r = value_as_int(right);
		vm_stack_push(M, value_int(l * r));
	    } else if (value_is_number(left) && value_is_number(right)) {
		double l, r;

		l = value_as_number(left);
		r = value_as_number(right);
		vm_stack_push(M, value_double(l * r));
	    } else {
		error(M, "operands not supported for '*'");
	    }
	} break;

	case OP_DIVIDE: {
	    Value left, right;

	    right = vm_stack_pop(M);
	    left = vm_stack_pop(M);
	    if (value_is_int(left) && value_is_int(right)) {
		int l, r;
		
		l = value_as_int(left);
		r = value_as_int(right);
		vm_stack_push(M, value_double(l / r));
	    } else if (value_is_number(left) && value_is_number(right)) {
		double l, r;

		l = value_as_number(left);
		r = value_as_number(right);
		vm_stack_push(M, value_double(l / r));
	    } else {
		error(M, "operands not supported for '/'");
	    }
	} break;

	case OP_GR: {
	    Value left, right;

	    right = vm_stack_pop(M);
	    left = vm_stack_pop(M);
	    if (value_is_int(left) && value_is_int(right)) {
		int l, r;
		
		l = value_as_int(left);
		r = value_as_int(right);
		vm_stack_push(M, value_double(l > r));
	    } else if (value_is_number(left) && value_is_number(right)) {
		double l, r;

		l = value_as_number(left);
		r = value_as_number(right);
		vm_stack_push(M, value_double(l > r));
	    } else {
		error(M, "operands not supported for '>'");
	    }
	} break;

	case OP_GREQ: {
	    Value left, right;

	    right = vm_stack_pop(M);
	    left = vm_stack_pop(M);
	    if (value_is_int(left) && value_is_int(right)) {
		int l, r;
		
		l = value_as_int(left);
		r = value_as_int(right);
		vm_stack_push(M, value_double(l >= r));
	    } else if (value_is_number(left) && value_is_number(right)) {
		double l, r;

		l = value_as_number(left);
		r = value_as_number(right);
		vm_stack_push(M, value_double(l >= r));
	    } else {
		error(M, "operands not supported for '>='");
	    }
	} break;

	case OP_LE: {
	    Value left, right;

	    right = vm_stack_pop(M);
	    left = vm_stack_pop(M);
	    if (value_is_int(left) && value_is_int(right)) {
		int l, r;
		
		l = value_as_int(left);
		r = value_as_int(right);
		vm_stack_push(M, value_double(l < r));
	    } else if (value_is_number(left) && value_is_number(right)) {
		double l, r;

		l = value_as_number(left);
		r = value_as_number(right);
		vm_stack_push(M, value_double(l < r));
	    } else {
		error(M, "operands not supported for '<'");
	    }
	} break;

	case OP_LEQ: {
	    Value left, right;

	    right = vm_stack_pop(M);
	    left = vm_stack_pop(M);
	    if (value_is_int(left) && value_is_int(right)) {
		int l, r;
		
		l = value_as_int(left);
		r = value_as_int(right);
		vm_stack_push(M, value_double(l <= r));
	    } else if (value_is_number(left) && value_is_number(right)) {
		double l, r;

		l = value_as_number(left);
		r = value_as_number(right);
		vm_stack_push(M, value_double(l <= r));
	    } else {
		error(M, "operands not supported for '<='");
	    }
	} break;

	case OP_EQ: {
	    Value left, right;
	    b32 result;

	    right = vm_stack_pop(M);
	    left = vm_stack_pop(M);
	    switch (left.type) {
	    case TYPE_NIL:
		result = right.type == TYPE_NIL; break;
		
	    case TYPE_CHAR:
		if (!value_is_char(right)) {
		    result = false;
		} else {
		    result = value_as_char(left) == value_as_char(right);
		}
		break;

	    case TYPE_BOOL:
		if (!value_is_bool(right)) {
		    result = false;
		} else {
		    result = value_as_bool(left) == value_as_bool(right);
		}
		break;

	    case TYPE_INT:
		if (value_is_int(right)) {
		    result = value_as_int(left) == value_as_int(right);
		} else if (value_is_double(right)) {
		    result = value_as_int(left) == value_as_double(right);
		} else {
		    result = false;
		}
		break;

	    case TYPE_DOUBLE:
		if (value_is_double(right)) {
		    result = value_as_double(left) == value_as_double(right);
		} else if (value_is_int(right)) {
		    result = value_as_double(left) == value_as_int(right);
		} else {
		    result = false;
		}
		break;

	    case TYPE_STRING:
		if (!value_is_string(right)) {
		    result = false;
		} else {
		    String *l, *r;
		    
		    l = value_as_string(left);
		    r = value_as_string(right);
		    if (l->length != r->length)
			result = false;
		    else
			result = memcmp(l->str, r->str, l->length) == 0;
		}
		break;

	    case TYPE_TABLE:
		if (!value_is_table(right)) {
		    result = false;
		} else {
		    result = value_as_table(left) == value_as_table(right);
		}
		break;

	    case TYPE_FUNCTION:
		if (!value_is_function(right)) {
		    result = false;
		} else {
		    result = value_as_function(left) == value_as_function(right);
		}
		break;

	    case TYPE_CFUNCTION:
		if (!value_is_cfunction(right)) {
		    result = false;
		} else {
		    result = value_as_cfunction(left) == value_as_cfunction(right);
		}
		break;

		vm_stack_push(M, value_bool(result));
	    }
	} break;

	case OP_NOT: {
	    Value value;
	    b32 truthy;

	    value = vm_stack_pop(M);
	    truthy = value_is_truthy(value);
	    vm_stack_push(M, value_bool(truthy));
	} break;

	case OP_BNOT: {
	    Value value;

	    value = vm_stack_pop(M);
	    if (!value_is_int(value))
		error(M, "'~' operator must be used on integer");
	    vm_stack_push(M, value_int(~value_as_int(value)));
	} break;

	case OP_AND: {
	    Value left, right;

	    right = vm_stack_pop(M);
	    left = vm_stack_pop(M);
	    vm_stack_push(
		M,
		value_bool(value_is_truthy(left) && value_is_truthy(right))
	    );
	} break;

	case OP_BAND: {
	    Value left, right;

	    right = vm_stack_pop(M);
	    left = vm_stack_pop(M);
	    if (!value_is_int(left) || !value_is_int(right))
		error(M, "'&' only supports integer operands");
	    vm_stack_push(
		M,
		value_int(value_as_int(left) & value_as_int(right))
	    );
	} break;

	case OP_OR: {
	    Value left, right;

	    right = vm_stack_pop(M);
	    left = vm_stack_pop(M);
	    vm_stack_push(
		M,
		value_bool(value_is_truthy(left) || value_is_truthy(right))
	    );
	} break;

	case OP_BOR: {
	    Value left, right;

	    right = vm_stack_pop(M);
	    left = vm_stack_pop(M);
	    if (!value_is_int(left) || !value_is_int(right))
		error(M, "'|' only supports integer operands");
	    vm_stack_push(
		M,
		value_int(value_as_int(left) | value_as_int(right))
	    );
	} break;

	case OP_NEGATE: {
	    Value value;

	    value = vm_stack_pop(M);
	    if (value_is_int(value)) {
		vm_stack_push(M, value_int(-value_as_int(value)));
	    } else if (value_is_double(value)) {
		vm_stack_push(M, value_double(-value_as_double(value)));
	    } else {
		error(M, "unary '-' only supports int or double");
	    }
	} break;

	case OP_LSHIFT: {
	    Value left, right;

	    right = vm_stack_pop(M);
	    left = vm_stack_pop(M);
	    if (!value_is_int(left) || !value_is_int(right))
		error(M, "'<<' only supports integer operands");
	    vm_stack_push(
		M,
		value_int(value_as_int(left) << value_as_int(right))
	    );
	} break;

	case OP_RSHIFT: {
	    Value left, right;

	    right = vm_stack_pop(M);
	    left = vm_stack_pop(M);
	    if (!value_is_int(left) || !value_is_int(right))
		error(M, "'>>' only supports integer operands");
	    vm_stack_push(
		M,
		value_int(value_as_int(left) >> value_as_int(right))
	    );
	} break;

	case OP_BXOR: {
	    Value left, right;

	    right = vm_stack_pop(M);
	    left = vm_stack_pop(M);
	    if (!value_is_int(left) || !value_is_int(right))
		error(M, "'^' only supports integer operands");
	    vm_stack_push(
		M,
		value_int(value_as_int(left) ^ value_as_int(right))
	    );
	} break;

	case OP_PUSH: {
	    u16 index;
	    Value value;

	    index = READ_WORD(M);
	    value = READ_CONSTANT(M, index);
	    vm_stack_push(M, value);
	} break;

	case OP_PUSH_NIL: {
	    vm_stack_push(M, value_nil());
	} break;

	case OP_PUSH_TRUE: {
	    vm_stack_push(M, value_bool(true));
	} break;

	case OP_PUSH_FALSE: {
	    vm_stack_push(M, value_bool(false));
	} break;

	case OP_POP: {
	    (void)vm_stack_pop(M);
	} break;

	case OP_POP_N: {
	    u16 i = READ_WORD(M);
	    while (i-- > 0) {
		(void)vm_stack_pop(M);
	    }
	} break;

	case OP_JUMP: {
	    u16 offset = READ_WORD(M);
	    M->frametop[-1].ip += offset;
	} break;

	case OP_JUMP_IF_FALSE: {
	    u16 offset;
	    Value value;

	    offset = READ_WORD(M);
	    value = vm_stack_pop(M);
	    if (!value_is_truthy(value))
		M->frametop[-1].ip += offset;
	} break;

	case OP_LOOP: {
	    u16 offset;

	    offset = READ_WORD(M);
	    M->frametop[-1].ip -= offset + 3;
	} break;

	case OP_DEFINE_GLOBAL: {
	    u16 index;
	    Value value;
	    String *identifier;

	    index = READ_WORD(M);
	    value = READ_CONSTANT(M, index);
	    LUNA_ASSERT(value_is_string(value));
	    identifier = value_as_string(value);
	    if (table_contains(M->globals, identifier, NULL))
		error(M, "global variable '%.*s' already exists",
		      (int)identifier->length,
		      identifier->str);
	    table_insert(M->L, M->globals, identifier, vm_stack_pop(M));
	} break;

	case OP_SET_GLOBAL: {
	    u16 index;
	    Value value;
	    String *identifier;

	    index = READ_WORD(M);
	    value = READ_CONSTANT(M, index);
	    LUNA_ASSERT(value_is_string(value));
	    identifier = value_as_string(value);
	    if (!table_contains(M->globals, identifier, NULL))
		error(M, "global variable '%.*s' not defined",
		      (int)identifier->length,
		      identifier->str);
	    table_insert(M->L, M->globals, identifier, vm_stack_peek(M, -1));
	} break;

	case OP_GET_GLOBAL: {
	    u16 index;
	    Value value;
	    String *identifier;
	    Value gvalue;

	    index = READ_WORD(M);
	    value = READ_CONSTANT(M, index);
	    LUNA_ASSERT(value_is_string(value));
	    identifier = value_as_string(value);
	    if (!table_contains(M->globals, identifier, &gvalue))
		error(M, "global variable '%.*s' not defined",
		      (int)identifier->length,
		      identifier->str);
	    vm_stack_push(M, gvalue);
	} break;

	case OP_SET_LOCAL: {
	    u16 index;
	    Value to;

	    index = READ_WORD(M);
	    to = vm_stack_peek(M, -1);
	    M->frametop[-1].slots[index] = to;
	} break;

	case OP_GET_LOCAL: {
	    u16 index;
	    Value value;

	    index = READ_WORD(M);
	    value = M->frametop[-1].slots[index];
	    vm_stack_push(M, value);
	} break;

	case OP_CALL: {
	    u16 arity = 0;
	    Value value = {0};
	    Function *function = NULL;

	    arity = READ_WORD(M);
	    value = vm_stack_peek(M, -1 - arity);
	    LUNA_ASSERT(value_is_function(value));
	    function = value_as_function(value);
	    if (arity != function->arity)
		error(M, "function '%s' accepts '%d' args, but got '%d'",
		      function->name->str, function->arity, arity);
	    push_frame(M, function);
	} break;

	case OP_TABLE_SET: {
	    Value tvalue, svalue, fvalue;
	    Table *table;

	    svalue = vm_stack_pop(M); /* Set to this value */
	    fvalue = vm_stack_pop(M); /* This field in */
	    if (!value_is_int(fvalue) && !value_is_string(fvalue))
		error(M, "tables can only be subscripted with int or string");
	    tvalue = vm_stack_pop(M); /* This table */
	    if (!value_is_table(tvalue))
		error(M, "expected table for subscript");
	    table = value_as_table(tvalue);

	    if (value_is_int(fvalue)) {
		int i = value_as_int(fvalue);
		table_insert_i(M->L, table, i, svalue);
	    } else if (value_is_string(svalue)) {
		String *s = value_as_string(svalue);
		table_insert(M->L, table, s, svalue);
	    }
	} break;

	case OP_TABLE_GET: {
	    Value tvalue, fvalue, value;
	    Table *table;

	    fvalue = vm_stack_pop(M);
	    if (!value_is_int(fvalue) && !value_is_string(fvalue))
		error(M, "tables can only be subscripted with int or string");
	    tvalue = vm_stack_pop(M);
	    if (!value_is_table(tvalue))
		error(M, "expected table for subscript");
	    table = value_as_table(tvalue);

	    if (value_is_int(fvalue)) {
		int i = value_as_int(fvalue);
		if (!table_contains_i(table, i, &value))
		    error(M, "table does not contain field '%d'", i);
	    } else if (value_is_string(fvalue)) {
		String *s = value_as_string(fvalue);
		if (!table_contains(table, s, &value))
		    error(M, "table does not contain field '%s'", s->str);
	    }
	    vm_stack_push(M, value);
	} break;

	case OP_TABLE_GET_WITH_SELF: {
	    Value tvalue, fvalue, value;
	    Table *table;

	    fvalue = vm_stack_pop(M);
	    if (!value_is_int(fvalue) && !value_is_string(fvalue))
		error(M, "tables can only be subscripted with int or string");
	    tvalue = vm_stack_pop(M);
	    if (!value_is_table(tvalue))
		error(M, "expected table for subscript");
	    table = value_as_table(tvalue);

	    if (value_is_int(fvalue)) {
		int i = value_as_int(fvalue);
		if (!table_contains_i(table, i, &value))
		    error(M, "table does not contain field '%d'", i);
	    } else if (value_is_string(fvalue)) {
		String *s = value_as_string(fvalue);
		if (!table_contains(table, s, &value))
		    error(M, "table does not contain field '%s'", s->str);
	    }
	    vm_stack_push(M, value);
	    vm_stack_push(M, tvalue);
	} break;

	case OP_RETURN: {
	    u16 arity = 0;
	    Value result = {0};

	    arity = READ_WORD(M);
	    result = vm_stack_pop(M); /* Return value */
	    while (arity-- > 0) { /* Arguments */
		(void)vm_stack_pop(M);
	    }
	    (void)vm_stack_pop(M); /* Function value */
	    vm_stack_push(M, result);
	    M->frametop--;
	    if (M->frametop == M->frames)
		return 0;
	} break;

	case OP_PRINT: {
	    Value value;
	    String *string;

	    value = vm_stack_pop(M);
	    string = value_as_string_repr(M->L, value);
	    fprintf(M->L->out, "%s\n", string->str);
	} break;

	default: {
	    error(M, "unhandled opcode '%d'", op);
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
    /*
    if (M->stacktop == M->stack) {
	error(M, "stack underflow!");
	}*/
    return *(--M->stacktop);
}

Value vm_stack_peek(VM *M, int offset) {
    return STACK_AT(M, offset);
}

static void print_stack_trace(VM *M) {
    int i;
    for (i = M->frametop - M->frames - 1; i >= 0; i--) {
	CallFrame frame = M->frames[i];
	Function *function = frame.function;
        ptrdiff_t ins = frame.ip - function->code - 1;
	int lineno = function->lines[ins];
	
	fprintf(M->L->out, "line %d: in function '%s'\n",
		lineno, function->name->str);
    }
}

/* Defintions */
void error(VM *M, const char *format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(M->L->out, format, args);
    fprintf(M->L->out, "\n");
    print_stack_trace(M);
    longjmp(M->error, 1);
}

void push_frame(VM *M, Function *f) {
    CallFrame frame = {0};
    frame.function = f;
    frame.ip = f->code;
    LUNA_ASSERT(f->arity >= 0);
    frame.slots = M->stacktop - f->arity - 1;
    if (M->frametop >= M->frames + MAX_RECURSION)
	error(M, "max recursion reached");
    *M->frametop++ = frame;
}
