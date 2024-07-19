#ifndef LUNA_LEXER_H
#define LUNA_LEXER_H

/**
 * Holds the state of the lexer
 */
typedef struct {
    const char *start; /* Start position of lexeme being parsed */
    const char *current; /* Current character being parsed */
    int lineno; /* Current line number */
} LexState;

/**
 * Initialize the LexState given a source string.
 */
void lexer_init(LexState *S, const char *src);

#define TOKENS \
    X(TOK_OPEN_PAREN),\
	X(TOK_CLOSE_PAREN),\
	X(TOK_OPEN_BRACKET),\
	X(TOK_CLOSE_BRACKET),\
	X(TOK_OPEN_BRACE),\
	X(TOK_CLOSE_BRACE),\
	X(TOK_SEMICOLON),\
	X(TOK_COLON),\
	X(TOK_DOT),\
	X(TOK_COMMA),\
	X(TOK_QUESTION),\
	\
	X(TOK_PLUS),\
	X(TOK_MINUS),\
	X(TOK_STAR),\
	X(TOK_STAR_STAR),\
	X(TOK_SLASH),\
	X(TOK_GREATER),\
	X(TOK_GREATER_GREATER),\
	X(TOK_GREATER_EQUAL),\
	X(TOK_LESS),\
	X(TOK_LESS_LESS),\
	X(TOK_LESS_EQUAL),\
	X(TOK_EQUAL),\
	X(TOK_EQUAL_EQUAL),\
	X(TOK_AMP),\
	X(TOK_AMP_AMP),\
	X(TOK_PIPE),\
X(TOK_PIPE_PIPE),\
			X(TOK_TILDE),\
			X(TOK_BANG),\
			X(TOK_BANG_EQUAL),\
			X(TOK_CARAT),\
			\
			X(TOK_LET),\
			X(TOK_RETURN),\
			X(TOK_FUNCTION),\
			X(TOK_IF),\
			X(TOK_ELSE),\
			X(TOK_WHILE),\
			X(TOK_FOR),\
			X(TOK_IN),\
			X(TOK_CONTINUE),\
			X(TOK_BREAK),\
			X(TOK_DO),\
	X(TOK_THEN),\
			X(TOK_END),\
	X(TOK_PRINT),\
			\
			X(TOK_IDENTIFIER),\
	X(TOK_NIL),\
	X(TOK_TRUE),\
	X(TOK_FALSE),\
			X(TOK_STRING),\
			X(TOK_INTEGER),\
			X(TOK_DOUBLE),\
			\
			X(TOK_EOF),\
X(TOK_ERROR)

#define X(tok) tok
typedef enum {
    TOKENS
} TokenType;
#undef X

const char *token_as_string(TokenType t);

typedef struct {
    TokenType type;
    const char *start;
    int length;
    int lineno;
} Token;

/**
 * Returns the next token in the stream.
 */
Token lexer_next(LexState *S);

#endif /* LUNA_LEXER_H */
