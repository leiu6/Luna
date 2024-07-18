#ifndef LUNA_STATE_H
#define LUNA_STATE_H

#ifdef __cplusplus
#define LUNA_API extern "C"
#else
#define LUNA_API extern
#endif

typedef struct Luna_State Luna_State;

/**
 * Create a new instance of the Luna interpreter. Must be released when done by
 * calling luna_state_delete().
 */
LUNA_API Luna_State *luna_state_new(void);

/**
 * Delete an instance of the Luna interpreter.
 */
LUNA_API void luna_state_delete(Luna_State *);

#define LUNA_RUNTIME_ERROR (1)
#define LUNA_COMPILE_ERROR (2)
#define LUNA_OOM_ERROR (3)

/**
 * Execute a string as source code.
 * Returns 0 if there was no error, or a nonzero error code otherwise.
 */
LUNA_API int luna_execute_string(Luna_State *L, const char *src);

#endif /* LUNA_STATE_H */
