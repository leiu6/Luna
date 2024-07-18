#include "object.h"

#include "state_internal.h"
#include <string.h>

Value value_nil(void) {
    Value v;
    v.type = TYPE_NIL;
    v.value.o = NULL;
    return v;
}

Value value_char(char c) {
    Value v;
    v.type = TYPE_CHAR;
    v.value.c = c;
    return v;
}

Value value_bool(b32 b) {
    Value v;
    LUNA_ASSERT(b == true || b == false);
    v.type = TYPE_BOOL;
    v.value.i = b;
    return v;
}

Value value_int(int i) {
    Value v;
    v.type = TYPE_INT;
    v.value.i = i;
    return v;
}

Value value_double(double d) {
    Value v;
    v.type = TYPE_DOUBLE;
    v.value.d = d;
    return v;
}

Value value_string(String *s) {
    Value v;
    LUNA_ASSERT(s != NULL);
    v.type = TYPE_STRING;
    v.value.o = AS_OBJECT(s);
    return v;
}

Value value_function(Function *f) {
    Value v;
    LUNA_ASSERT(f != NULL);
    v.type = TYPE_FUNCTION;
    v.value.o = AS_OBJECT(f);
    return v;
}

Value value_table(Table *t) {
    Value v;
    LUNA_ASSERT(t != NULL);
    v.type = TYPE_TABLE;
    v.value.o = AS_OBJECT(t);
    return v;
}

Object *object_alloc(Luna_State *L, size_t size) {
    Object *object = NULL;
    
    LUNA_ASSERT(size > sizeof(Object));
    object = ALLOCATE(L, size);
    object->next = NULL;
    object->marked = false;
    /* TODO: add to GC list */
    return object;
}

String *string_alloc(Luna_State *L, const char *str) {
    return string_alloc_l(L, str, strlen(str));
}

String *string_alloc_l(Luna_State *L, const char *str, size_t len) {
    String *string = AS_STRING(object_alloc(L, sizeof(String)));
    string->length = len;
    string->str = ARRAY_ALLOCATE(L, char, string->length + 1);
    memcpy(string->str, str, string->length);
    string->str[string->length] = 0;
    return string;
}

String *string_alloc_concat(
    Luna_State *L,
    const char *left,
    size_t leftlen,
    const char *right,
    size_t rightlen) {
    String *string = AS_STRING(object_alloc(L, sizeof(String)));
    string->length = leftlen + rightlen;
    string->str = ARRAY_ALLOCATE(L, char, string->length + 1);
    memcpy(string->str, left, leftlen);
    memcpy(string->str + leftlen, right, rightlen);
    string->str[string->length] = 0;
    return string;
}


#define CODE_INIT 128
#define CONSTANTS_INIT 16

Function *function_alloc(Luna_State *L, String *name) {
    Function *function = AS_FUNCTION(object_alloc(L, sizeof(Function)));
    function->name = name;
    function->code = ARRAY_ALLOCATE(L, u8, CODE_INIT);
    function->code_allocated = CODE_INIT;
    function->code_size = 0;
    function->constants = ARRAY_ALLOCATE(L, Value, CONSTANTS_INIT);
    function->constants_allocated = CONSTANTS_INIT;
    function->constants_size = 0;
    function->arity = -1; /* Unset */
    return function;
}

void function_write_byte(Luna_State *L, Function *function, u8 byte) {
    if (function->code_size + 1 > function->code_allocated) {
	function->code = ARRAY_GROW(
	    L,
	    u8,
	    function->code,
	    function->code_allocated,
	    2 * function->code_allocated);
    }
    function->code[function->code_size++] = byte;
}

u16 function_write_constant(Luna_State *L, Function *function, Value constant) {
    if (function->constants_size + 1 > function->constants_allocated) {
	function->constants = ARRAY_GROW(
	    L,
	    Value,
	    function->constants,
	    function->constants_allocated,
	    2 * function->constants_allocated);
    }
    function->constants[function->constants_size++] = constant;
    LUNA_ASSERT(function->constants_size <= 0xFFFF);
    return function->constants_size - 1;
}

Table *table_alloc(Luna_State *L) {
    Table *table = AS_TABLE(object_alloc(L, sizeof(Table)));
    return table;
}
