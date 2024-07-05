#include "lexer.h"

#include "defs.h"
#include <assert.h>
#include <string.h>

/* Forward */

/**
 * Advances the lexer by 1 character and returns the current one.
 */
static char advance(LexState *S);

/**
 * Returns the current character like advance but does not move forward.
 */
static char peek(LexState *S);

/**
 * Checks if the current character is 'expected'. If so, the lexer advances and
 * true is returned. Otherwise, false is simply returned and the lexer does not
 * advance.
 */
static b32 match(LexState *S, char expected);

/**
 * Skips all whitespace characters and returns the first one that is not
 * whitespace in the source string.
 */
static char get_first_non_whitespace(LexState *S);

/**
 * Rolling my own functions for checking what type of char it is.
 */
static b32 is_numerical(char c);
static b32 is_alphabetical(char c);
static b32 is_alphanumerical(char c);

/**
 * Creates a token for the current lexical context.
 */
static Token make_token(LexState *S, TokenType t);

/**
 * Parses a string literal and generates a lexer token from it.
 */
static Token make_string_token(LexState *S);

/**
 * Parses a number and creates a lexer token.
 */
static Token make_number_token(LexState *S);

/**
 * Parses an identifier and determines whether it is a reserved keyword or not.
 * The appropriate lexer token is then generated.
 */
static Token make_identifier_token(LexState *S);

/**
 * Creates a token representing an error. This will be handled by the parser
 * later and printed.
 */
static Token make_error_token(LexState *S);


void lexer_init(LexState *S, const char *src) {
    S->start = src;
    S->current = src;
    S->lineno = 1;
}

const char *token_as_string(TokenType t) {
#define X(tok) #tok
    static const char *strs[] = {TOKENS};
#undef X
    return strs[t];
}

Token lexer_next(LexState *S) {
    char c = get_first_non_whitespace(S);
    switch (c) {
    case '(': return make_token(S, TOK_OPEN_PAREN);
    case ')': return make_token(S, TOK_CLOSE_PAREN);
    case '[': return make_token(S, TOK_OPEN_BRACKET);
    case ']': return make_token(S, TOK_CLOSE_BRACKET);
    case '{': return make_token(S, TOK_OPEN_BRACE);
    case '}': return make_token(S, TOK_CLOSE_BRACE);
    case ';': return make_token(S, TOK_SEMICOLON);
    case ':': return make_token(S, TOK_COLON);
    case '.': return make_token(S, TOK_DOT);
    case ',': return make_token(S, TOK_COMMA);
    case '+': return make_token(S, TOK_PLUS);
    case '-': return make_token(S, TOK_MINUS);
    case '*': return make_token(S, match(S, '*') ? TOK_STAR_STAR : TOK_STAR);
    case '/': return make_token(S, TOK_SLASH);
    case '>': return make_token(S, match(S, '>') ? TOK_GREATER_GREATER
				: (match(S, '=') ? TOK_GREATER_EQUAL
				   : TOK_GREATER));
    case '<': return make_token(S, match(S, '<') ? TOK_LESS_LESS
				: (match(S, '=') ? TOK_LESS_EQUAL
				   : TOK_LESS));
    case '=': return make_token(S, match(S, '=') ? TOK_EQUAL_EQUAL : TOK_EQUAL);
    case '&': return make_token(S, match(S, '&') ? TOK_AMP_AMP : TOK_AMP);
    case '|': return make_token(S, match(S, '|') ? TOK_PIPE_PIPE : TOK_PIPE);
    case '~': return make_token(S, TOK_TILDE);
    case '!': return make_token(S, match(S, '=') ? TOK_BANG_EQUAL : TOK_BANG);
    case '^': return make_token(S, TOK_CARAT);
    case '\0': return make_token(S, TOK_EOF);
    case '"': return make_string_token(S);
    default: {
        if (is_numerical(c)) {
	    return make_number_token(S);
	} else if (is_alphabetical(c) || c == '_') {
	    return make_identifier_token(S);
	} else {
	    return make_error_token(S);
	}
    }
    }
}

/* Implementation */
char advance(LexState *S) {
    char c = *S->current++;
    if (c == '\n') {
	S->lineno++;
    }
    return c;
}

char peek(LexState *S) {
    return *S->current;
}

b32 match(LexState *S, char expected) {
    if (peek(S) == expected) {
	(void)advance(S);
	return true;
    }
    return false;
}

Token make_token(LexState *S, TokenType t) {
    Token tok;
    tok.type = t;
    tok.start = S->start;
    tok.length = S->current - S->start;
    tok.lineno = S->lineno;
    return tok;
}

Token make_string_token(LexState *S) {
    while (peek(S) != '"') {
	(void)advance(S);
    }
    return make_token(S, TOK_STRING);
}

Token make_number_token(LexState *S) {
    while (is_numerical(peek(S))) {
	(void)advance(S);
    }
    if (!match(S, '.')) {
	return make_token(S, TOK_INTEGER);
    }
    while (is_numerical(peek(S))) {
	(void)advance(S);
    }
    return make_token(S, TOK_DOUBLE);
}

/**
 * Represents a keyword defintion. Use the K macro to create more.
 */
typedef struct {
    const char *keyword;
    size_t length;
    TokenType type;
} Keyword;

/**
 * Get the length of a string literal at compile time.
 */
#define STATIC_STRLEN(strlit) \
    (ARRAYSIZE(strlit) - 1)

/**
 * Construct a new reserved keyword definition.
 * keyword: string literal of the keyword to match
 * type: the TokenType to return if the keyword is matched
 */
#define K(keyword, type) \
    {keyword, STATIC_STRLEN(keyword), type}

/**
 * List of reserved keywords
 */
static Keyword keywords[] = {
    K("let", TOK_LET),
    K("return", TOK_RETURN),
    K("function", TOK_FUNCTION),
    K("if", TOK_IF),
    K("else", TOK_ELSE),
    K("while", TOK_WHILE),
    K("for", TOK_FOR),
    K("in", TOK_IN),
    K("continue", TOK_CONTINUE),
    K("break", TOK_BREAK)
};

/**
 * Determines whether the current parsed lexeme is a reserved keyword or an
 * identifier. Returns the appropriate token type.
 */
static TokenType determine_keyword_or_identifier(LexState *S) {
    size_t i;
    for (i = 0; i < ARRAYSIZE(keywords); i++) {
	Keyword keyword = keywords[i];
	if (keyword.length != S->current - S->start)
	    continue;
	if (memcmp(keyword.keyword, S->start, keyword.length) == 0) {
	    return keyword.type;
	}
    }
    return TOK_IDENTIFIER;
}

Token make_identifier_token(LexState *S) {
    while (is_alphanumerical(peek(S)) || peek(S) == '_') {
	(void)advance(S);
    }
    return make_token(S, determine_keyword_or_identifier(S));
}

Token make_error_token(LexState *S) {
    return make_token(S, TOK_ERROR);
}

char get_first_non_whitespace(LexState *S) {
    char c;
    do {
	S->start = S->current;
	c = advance(S);
    } while (c == ' ' || c == '\n' || c == '\t' || c == '\r');
    return c;
}

b32 is_numerical(char c) {
    return (c >= '0') && (c <= '9');
}

b32 is_alphabetical(char c) {
    return ((c >= 'A') && (c <= 'Z'))
	|| ((c >= 'a') && (c <= 'z'));
}

b32 is_alphanumerical(char c) {
    return is_numerical(c) || is_alphabetical(c);
}
