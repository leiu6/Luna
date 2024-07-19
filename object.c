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

b32 value_is_truthy(Value v) {
    switch (v.type) {
    case TYPE_NIL: return false;
    case TYPE_CHAR: return value_as_char(v) != '\0';
    case TYPE_BOOL: return value_as_bool(v);
    case TYPE_INT: return value_as_int(v) ? true : false;
    case TYPE_DOUBLE: return value_as_double(v) ? true : false;
    case TYPE_STRING: return true;
    case TYPE_TABLE: return true;
    case TYPE_FUNCTION: return true;
    case TYPE_CFUNCTION: return true;
    default: LUNA_ASSERT(0 && "unreachable");
    }
}

String *value_as_string_repr(Luna_State *L, Value v) {
    switch (v.type) {
    case TYPE_NIL: return string_alloc(L, "nil");
    case TYPE_BOOL: return string_alloc(L, value_as_bool(v) ? "true" : "false");
    case TYPE_CHAR: {
	String *s = string_reserve(L, 1);
	s->str[0] = value_as_char(v);
	return s;
    }
    case TYPE_INT: {
	String *s = string_reserve(L, 11);
	size_t write = sprintf(s->str, "%d", value_as_int(v));
	LUNA_ASSERT(write <= 11);
	return s;
    }
    case TYPE_DOUBLE: {
	String *s = string_reserve(L, 11);
	size_t write = sprintf(s->str, "%f", value_as_double(v));
	LUNA_ASSERT(write <= 11);
	return s;
    }
    case TYPE_STRING: return value_as_string(v);
    case TYPE_TABLE: return string_alloc(L, "<table>");
    case TYPE_FUNCTION: return string_alloc(L, "<function>");
    case TYPE_CFUNCTION: return string_alloc(L, "<cfunction>");
    default: LUNA_ASSERT(0 && "unreachable");
    }
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

String *string_reserve(Luna_State *L, size_t length) {
    String *string = AS_STRING(object_alloc(L, sizeof(String)));
    string->length = length;
    string->str = ARRAY_ALLOCATE(L, char, string->length + 1);
    memset(string->str, 0x00, string->length + 1);
    return string;
}

String *string_alloc(Luna_State *L, const char *str) {
    return string_alloc_l(L, str, strlen(str));
}

String *string_alloc_l(Luna_State *L, const char *str, size_t len) {
    String *string = string_reserve(L, len);
    memcpy(string->str, str, string->length);
    return string;
}

String *string_alloc_concat(
    Luna_State *L,
    const char *left,
    size_t leftlen,
    const char *right,
    size_t rightlen) {
    String *string = string_reserve(L, leftlen + rightlen);
    memcpy(string->str, left, leftlen);
    memcpy(string->str + leftlen, right, rightlen);
    return string;
}

/* Functions */

#define CODE_INIT 128
#define CONSTANTS_INIT 16

Function *function_alloc(Luna_State *L, String *name) {
    Function *function = AS_FUNCTION(object_alloc(L, sizeof(Function)));
    function->name = name;
    function->code = ARRAY_ALLOCATE(L, u8, CODE_INIT);
    function->lines = ARRAY_ALLOCATE(L, int, CODE_INIT);
    function->code_allocated = CODE_INIT;
    function->code_size = 0;
    function->constants = ARRAY_ALLOCATE(L, Value, CONSTANTS_INIT);
    function->constants_allocated = CONSTANTS_INIT;
    function->constants_size = 0;
    function->arity = -1; /* Unset */
    return function;
}

void function_write_byte(Luna_State *L, Function *function, u8 byte, int lineno) {
    if (function->code_size + 1 > function->code_allocated) {
	function->code = ARRAY_GROW(
	    L,
	    u8,
	    function->code,
	    function->code_allocated,
	    2 * function->code_allocated);
	function->lines = ARRAY_GROW(
	    L,
	    int,
	    function->lines,
	    function->code_allocated,
	    2 * function->code_allocated);
	function->code_allocated *= 2;
    }
    function->lines[function->code_size] = lineno;
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
	function->constants_allocated *= 2;
    }
    function->constants[function->constants_size++] = constant;
    LUNA_ASSERT(function->constants_size <= 0xFFFF);
    return function->constants_size - 1;
}

/* Tables */

/**
 * Computes the hash of a given chunk of memory
 *
 * From: https://craftinginterpreters.com/hash-tables.html
 */
static u32 table_hash(void *memory, int length) {
  u32 hash = 2166136261u;
  char *data = memory;
  int i;
  
  for (i = 0; i < length; i++) {
    hash ^= (u8)data[i];
    hash *= 16777619;
  }
  return hash;
}

static u32 table_hash_cstring(const char *str) {
    return table_hash(str, strlen(str));
}

static u32 table_hash_string(String *str) {
    return table_hash(str->str, str->length);
}

static u32 table_hash_int(int i) {
    return table_hash(&i, sizeof(int));
}

static void table_insert_at_end(
    Luna_State *L, TBucket *bucket, u32 key, Value v) {
    TBucket *cur = bucket;
    TBucket *prev = NULL;

    for (;;) {
	if (!cur) {
	    TBucket *insert = ALLOCATE(L, sizeof(TBucket));
	    insert->hash = key;
	    insert->val = v;
	    insert->next = NULL;
	    LUNA_ASSERT(prev);
	    prev->next = insert;
	    return;
	}

	prev = cur;
	cur = cur->next;
    }
}

#define getbucket(table, key) \
    (table->buckets[key % TABLE_BUCKET_COUNT])

/**
 * Given a u32 hash, inserts a value into the table.
 */
static void table_insert_hash(Luna_State *L, Table *table, u32 key, Value v) {
    if (!getbucket(table, key)) {
	getbucket(table, key) = ALLOCATE(L, sizeof(TBucket));
	getbucket(table, key)->hash = key;
	getbucket(table, key)->val = v;
	getbucket(table, key)->next = NULL;
	return;
    } else {
	table_insert_at_end(L, getbucket(table, key), key, v);
    }
}

TBucket *table_find_in_bucket(TBucket *bucket, u32 hash) {
    TBucket *cur = bucket;

    for (;;) {
	if (!cur)
	    return NULL;
	if (cur->hash == hash)
	    return cur;
	cur = cur->next;
    }
}

b32 table_contains_hash(Table *table, u32 hash, Value *v) {
    TBucket *bucket = getbucket(table, hash);
    TBucket *found = table_find_in_bucket(bucket, hash);
    if (!found)
	return false;
    if (v)
	*v = found->val;
    return true;
}

Table *table_alloc(Luna_State *L) {
    Table *table = AS_TABLE(object_alloc(L, sizeof(Table)));
    memset(table->buckets, 0x00, sizeof(TBucket *) * TABLE_BUCKET_COUNT);
    return table;
}

b32 table_contains(Table *table, String *key, Value *v) {
    return table_contains_hash(table, table_hash_string(key), v);
}

b32 table_contains_s(Table *table, const char *key, Value *v) {
    return table_contains_hash(table, table_hash_cstring(key), v);
}

b32 table_contains_i(Table *table, int i, Value *v) {
    return table_contains_hash(table, table_hash_int(i), v);
}

void table_insert(Luna_State *L, Table *table, String *key, Value v) {
    table_insert_hash(L, table, table_hash_string(key), v);
}

void table_insert_s(Luna_State *L, Table *table, const char *key, Value v) {
    table_insert_hash(L, table, table_hash_cstring(key), v);
}

void table_insert_i(Luna_State *L, Table *table, int key, Value v) {
    table_insert_hash(L, table, table_hash_int(key), v);
}
