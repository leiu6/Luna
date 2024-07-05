#include <stdio.h>

#include "lexer.h"

static void print_token(Token t) {
    printf("Token: \n");
    printf("Type: '%s'\n", token_as_string(t.type));
    printf("Lexeme: %.*s\n", t.length, t.start);
    printf("Lineno: %d\n", t.lineno);
    printf("\n");
}

int main(void) {
    LexState lexer = {0};
    lexer_init(&lexer, "let my_var = 3.005;");

    for (;;) {
	Token tok = lexer_next(&lexer);
	print_token(tok);

	if (tok.type == TOK_EOF) {
	    break;
	}
    }
    
    return 0;
}
