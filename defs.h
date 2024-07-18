#ifndef LUNA_DEFS_H
#define LUNA_DEFS_H

#include <assert.h>
#include <stddef.h>

#ifndef LUNA_ASSERT
#define LUNA_ASSERT(cond) assert(cond)
#endif

/**
 * These type definitions will need to be changed for different platforms
 */

typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long u64;

typedef signed char i8;
typedef short i16;
typedef int i32;
typedef long i64;

typedef size_t usize;
typedef ptrdiff_t ssize;

typedef i32 b32;

#ifndef true
#define true 1
#endif

#ifndef false
#define false 0
#endif

/**
 * Returns the number of elements in a static array.
 */
#define ARRAYSIZE(array) \
    (sizeof(array) / sizeof(array[0]))

#ifdef __GNUC__
#define NORETURN __attribute__((noreturn))
#elif __clang__
#define NORETURN __attribute__((noreturn))
#elif _MSC_VER
#define NORETURN __declspec(noreturn)
#else
#define NORETURN
#endif

#endif /* LUNA_DEFS_H */
