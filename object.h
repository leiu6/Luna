#ifndef LUNA_OBJECT_H
#define LUNA_OBJECT_H

#include "defs.h"
#include "state.h"

typedef struct Object Object;

typedef enum {
    TYPE_NIL,
    TYPE_CHAR,
    TYPE_BOOL,
    TYPE_INT,
    TYPE_DOUBLE,
    TYPE_STRING,
    TYPE_TABLE,
    TYPE_FUNCTION,
    TYPE_CFUNCTION
} Type;

typedef int (*Luna_Callback)(Luna_State *L, int argc);

/**
 * Represents a value in Luna. Has a type field and a union which can either
 * hold a stack value like char, integer, boolean, or it can house a reference
 * to a garbage collected value like a string or a table.
 */
typedef struct {
    Type type;

    union {
	char c;
	int i;
	double d;
	Object *o;
	Luna_Callback cf;
    } value;
} Value;

struct Object {
    Object *next;
    b32 marked;
};

/**
 * Converts garbage collected object to Object struct
 */
#define AS_OBJECT(o) ((Object *)o)

/**
 * Create a garbage collected region of memory.
 */
Object *object_alloc(Luna_State *L, size_t size);

/**
 * All garbage collected objects must have OBJECT_HEAD as the first field.
 */
#define OBJECT_HEAD \
    Object ob_head;

typedef struct {
    OBJECT_HEAD

    size_t length;
    char *str;
} String;

/**
 * Converts Object * to String *
 */
#define AS_STRING(o) ((String *)o)

/**
 * Allocate an existing string of length + 1 bytes set to 0.
 */
String *string_reserve(Luna_State *L, size_t length);

/**
 * Create a string by copying the null terminated 'str'.
 */
String *string_alloc(Luna_State *L, const char *str);

String *string_alloc_l(Luna_State *L, const char *str, size_t len);

String *string_alloc_concat(
    Luna_State *L,
    const char *left,
    size_t leftlen,
    const char *right,
    size_t rightlen);

typedef struct {
    OBJECT_HEAD

    String *name;
    u8 *code;
    int *lines;
    size_t code_allocated;
    size_t code_size;
    Value *constants;
    size_t constants_allocated;
    size_t constants_size;
    int arity;
} Function;

#define AS_FUNCTION(o) ((Function *)o)

/**
 * Creates a new function object. Arity is set to -1 by default and must be
 * configured before calling.
 */
Function *function_alloc(Luna_State *L, String *name);

/**
 * Write a byte to the functions code.
 */
void function_write_byte(Luna_State *L, Function *function, u8 byte, int lineno);

/**
 * Add a constant to the constants table of the function. Returns the index of
 * the constant.
 */
u16 function_write_constant(Luna_State *L, Function *function, Value constant);

typedef struct TBucket {
    struct TBucket *next;
    Value val;
    u32 hash;
} TBucket;

#define TABLE_BUCKET_COUNT (0xFF + 1)

/**
 * Represents a hash table. This is the standard datastructure for Luna. Tables
 * can be indexed by integer or string.
 */
typedef struct {
    OBJECT_HEAD

    TBucket *buckets[TABLE_BUCKET_COUNT];
} Table;

#define AS_TABLE(o) ((Table *)o)

/**
 * Generates an empty table.
 */
Table *table_alloc(Luna_State *L);

b32 table_contains(Table *table, String *key, Value *v);
b32 table_contains_s(Table *table, const char *key, Value *v);
b32 table_contains_i(Table *table, int key, Value *v);

void table_insert(Luna_State *L, Table *table, String *key, Value v);
void table_insert_s(Luna_State *L, Table *table, const char *key, Value v);
void table_insert_i(Luna_State *L, Table *table, int key, Value v);

Value value_nil(void);
Value value_char(char);
Value value_bool(b32);
Value value_int(int);
Value value_double(double);
Value value_string(String *);
Value value_function(Function *);
Value value_table(Table *);

#define value_is_nil(v) (v.type == TYPE_NIL)
#define value_is_char(v) (v.type == TYPE_CHAR)
#define value_is_bool(v) (v.type == TYPE_BOOL)
#define value_is_int(v) (v.type == TYPE_INT)
#define value_is_double(v) (v.type == TYPE_DOUBLE)
#define value_is_number(v) (value_is_int(v) || value_is_double(v))
#define value_is_string(v) (v.type == TYPE_STRING)
#define value_is_function(v) (v.type == TYPE_FUNCTION)
#define value_is_cfunction(v) (v.type == TYPE_CFUNCTION)
#define value_is_table(v) (v.type == TYPE_TABLE)
b32 value_is_truthy(Value v);

#define value_as_char(v) (v.value.c)
#define value_as_bool(v) (v.value.i)
#define value_as_int(v) (v.value.i)
#define value_as_double(v) (v.value.d)
#define value_as_number(v) \
    ((value_is_double(v)) ? value_as_double(v) : value_as_int(v))
#define value_as_string(v) (AS_STRING(v.value.o))
#define value_as_function(v) (AS_FUNCTION(v.value.o))
#define value_as_cfunction(v) (v.value.cf)
String *value_as_string_repr(Luna_State *L, Value v);

#define value_as_table(v) (AS_TABLE(v.value.o))

#endif /* LUNA_OBJECT_H */
