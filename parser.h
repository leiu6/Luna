#ifndef LUNA_PARSER_H
#define LUNA_PARSER_H

#include "lexer.h"
#include "object.h"
#include "state_internal.h"
#include <setjmp.h>

/**
 * Represents a local variable entry.
 */
typedef struct {
    const char *name; /* Name of the local variable */
    int length; /* Length of the name */
    int depth; /* Scope depth of the variable */
} Local;

/* Max amount of local variables allowed in a function */
#define MAX_LOCALS (0xFF)

/**
 * Represents the context of the current function being compiled.
 */
typedef struct Compiler {
    struct Compiler *enclosing; /* Enclosing function being parsed */
    Function *function; /* Current function being parsed */
    int depth; /* Scope depth */
    Local locals[MAX_LOCALS]; /* Local variable lookup table */
    int num_locals; /* Number of local variables in this function */
} Compiler;

typedef struct {
    Luna_State *L; /* Interpreter instance */
    LexState lexer; /* Lexer instance */
    Token current; /* Current token being processed */
    Token previous; /* Previous token that was processed */
    Compiler *current_compiler; /* Current function compiler being processed */
    b32 had_error; /* True if an error ocurred during parsing */
    jmp_buf panic; /* Jump point to skip on error to valid context */
} ParseState;

/**
 * Initialize the parser with a source string.
 */
void parser_init(Luna_State *L, ParseState *S, const char *src);

/**
 * Parses the source string and returns a reference to the function object for
 * the entire program. If there was an error, NULL will be returned instead.
 */
Function *parser_parse(ParseState *S);

#endif /* LUNA_PARSER_H */
