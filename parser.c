#include "parser.h"

#include "opcodes.h"
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

/* Forward */

/* Navigation */

/**
 * Advances the parser by one tokken.
 */
static void advance(ParseState *S);

/**
 * Determine if the current token is of type 't' or not.
 */
static b32 peek(ParseState *S, TokenType t);

/**
 * Determines if the current token is of type 't'. If so, return true and
 * advance. Otherwise, return false.
 */
static b32 match(ParseState *S, TokenType t);

/**
 * Determines if the current token is of type 't'. If so, the parser advances.
 * Otherwise, the formatted error message is printed.
 */
static void consume(ParseState *S, TokenType t, const char *format, ...);

/**
 * Converts the identifier token into a string.
 */
static String *token_stringify(ParseState *S, Token tok);


/* Error reporting */

/* Do not call these directly */
NORETURN static void vferror(
    ParseState *S, int line, const char *format, va_list args);

/**
 * For reporting an error with the current token.
 */
NORETURN static void error_current(ParseState *S, const char *format, ...);

/**
 * For reporting an error with the previous token.
 */
NORETURN static void error_previous(ParseState *S, const char *format, ...);


/* Codegen */
/**
 * Initializes a compiler, which represents the scope of the current function
 * being parsed.
 */
static void begin_compiler(ParseState *S, Compiler *C, String *name);

/**
 * Ends the current function compiler, and returns the parsed function.
 */
static Function *end_compiler(ParseState *S);

/**
 * Indends and dedends the scope.
 */
static void begin_scope(ParseState *S);
static void end_scope(ParseState *S);

/* Variables */

/**
 * Declares the existence of an uninitialized variable.
 */
static void declare_variable(ParseState *S);

/**
 * Sets the value for a variable.
 */
static void define_variable(ParseState *S, Token identifier);

/**
 * Determines whether a local variable exists. If so, returns the index of it on
 * the stack. If not, returns -1.
 */
static int resolve_local(ParseState *S, Token identifier);

/**
 * Writes a single byte to the current function.
 */
static void emit_byte(ParseState *S, u8 byte);

/**
 * Emits two bytes to the function.
 */
static void emit_word(ParseState *S, u16 word);

/**
 * Adds a value to the constants table for the current function and inserts the
 * address of as two bytes in the code of the function.
 */
static void emit_constant(ParseState *S, Value constant);

/**
 * Emits a jump instruction represented by 'type'. Returns the position of the
 * jump offset argument. This is used later by patch_jump to set the jump
 * offset to the proper amount.
 */
static size_t emit_jump(ParseState *S, Opcode type);

/**
 * Gets the current offset position of the code being emitted. This can be
 * supplied layer to emit_loop.
 */
static size_t get_code_offset(ParseState *S);

/**
 * Emits an OP_LOOP instruction that will do a negative jump offset to the
 * location 'where'.
 */
static void emit_loop(ParseState *S, u16 where);

/**
 * Edits the argument for a jump instruction. Sets the jump offset such that it
 * will jump to the current position of code being emitted.
 */
static void patch_jump(ParseState *S, size_t where);

/* Parse levels */

/**
 * Top parsing level. Parses the entire program and consumes the TOK_EOF.
 */
static void parse_program(ParseState *S);

/**
 * Declarations
 */
static void parse_declaration(ParseState *S);
static void parse_block(ParseState *S);
static void parse_variable_declaration(ParseState *S);
static void parse_function_declaration(ParseState *S);

/**
 * Statements
 */
static void parse_statement(ParseState *S);
static void parse_if_statement(ParseState *S);
static void parse_while_statement(ParseState *S);
static void parse_return_statement(ParseState *S);
static void parse_expression_statement(ParseState *S);
static void parse_print_statement(ParseState *S);

/**
 * Expressions
 */
static void parse_expression(ParseState *S);

void parser_init(Luna_State *L, ParseState *S, const char *src) {
    S->L = L;
    lexer_init(&S->lexer, src);
    S->had_error = false;
    /* current, previous, and panic will be set later */
}

Function *parser_parse(ParseState *S) {
    Compiler compiler = {0};
    Function *result = NULL;

    if (setjmp(S->panic) != 0) {
	return NULL; /* TODO: skip to next valid */
    }
    begin_compiler(S, &compiler, string_alloc(S->L, "<script>"));
    compiler.function->arity = 0;
    parse_program(S);
    result = end_compiler(S);
    return (!S->had_error) ? result : NULL;
}


/* Defintions */

void advance(ParseState *S) {
    S->previous = S->current;
    for (;;) {
	S->current = lexer_next(&S->lexer);
	if (!peek(S, TOK_ERROR))
	    break;
	error_current(
	    S, "unexpected '%.*s'", S->current.length, S->current.start);
    }
}

b32 peek(ParseState *S, TokenType t) {
    return S->current.type == t;
}

b32 match(ParseState *S, TokenType t) {
    if (peek(S, t)) {
	advance(S);
	return true;
    }
    return false;
}

void consume(ParseState *S, TokenType t, const char *format, ...) {
    va_list args;
    if (match(S, t))
	return;
    va_start(args, format);
    vferror(S, S->current.lineno, format, args);
    LUNA_ASSERT(0 && "unreachable");
}

String *token_stringify(ParseState *S, Token tok) {
    String *string = NULL;
    
    LUNA_ASSERT(tok.type == TOK_IDENTIFIER);
    string = string_alloc_l(S->L, tok.start, tok.length);
    return string;
}

void vferror(ParseState *S, int line, const char *format, va_list args) {
    fprintf(S->L->error, "%d: ", line);
    vfprintf(S->L->error, format, args);
    fprintf(S->L->error, "\n");
    longjmp(S->panic, 1);
}

void error_current(ParseState *S, const char *format, ...) {
    va_list args;
    va_start(args, format);
    vferror(S, S->current.lineno, format, args);
    LUNA_ASSERT(0 && "unreachable");
}

void error_previous(ParseState *S, const char *format, ...) {
    va_list args;
    va_start(args, format);
    vferror(S, S->previous.lineno, format, args);
    LUNA_ASSERT(0 && "unreachable");
}

void begin_compiler(ParseState *S, Compiler *C, String *name) {
    C->enclosing = S->current_compiler;
    C->function = function_alloc(S->L, name);
    C->depth = 0;
    /* First slot reserved */
    C->locals[0].name = "";
    C->locals[0].length = 0;
    C->locals[0].depth = -1;
    C->num_locals = 1;
    S->current_compiler = C;
}

#ifndef NDEBUG
static const char *opcode_as_string(Opcode op) {
#define X(op) #op
#define X_ARG(op) #op
    static const char *ops[] = {OPCODES};
#undef X
#undef X_ARG
    return ops[op];
}

static b32 opcode_has_arg(Opcode op) {
#define X(op) false
#define X_ARG(op) true
    b32 has_arg[] = {OPCODES};
#undef X
#undef X_ARG
	return has_arg[op];
}

static u16 bytes_join(u8 most, u8 least) {
    return (most << 8) + least;
}

/**
 * For debugging purposes, disassembles the function and prints it to terminal.
 */
static void disassemble_function(Luna_State *L, Function *f) {
    size_t i;
    
    fprintf(L->out, "Disassembling Function: %s\n", f->name->str);
    for (i = 0; i < f->code_size; i++) {
	u8 opcode = f->code[i];

	if (opcode_has_arg(opcode)) {
	    fprintf(
		L->out,
		"%04d: %s, %d\n",
		(int)i,
		opcode_as_string(opcode), bytes_join(
		    f->code[i + 1], f->code[i + 2]));
	    i += 2;
	} else {
	    fprintf(
		L->out,
		"%04d: %s\n",
		(int)i,
		opcode_as_string(opcode)
     		);
	}
    }
}
#endif /* ifndef NDEBUG */

Function *end_compiler(ParseState *S) {
    Function *result = NULL;
    
    emit_byte(S, OP_RETURN);
    emit_word(S, S->current_compiler->function->arity);
    result = S->current_compiler->function;
    if (S->current_compiler->enclosing)
	S->current_compiler = S->current_compiler->enclosing;
#ifndef NDEBUG
    disassemble_function(S->L, result);
#endif 
    return result;
}

void begin_scope(ParseState *S) {
    S->current_compiler->depth++;
}

void end_scope(ParseState *S) {
    int i = 0;
    Compiler *c = S->current_compiler;

    c->depth--;
    while (c->num_locals > 0 && c->locals[c->num_locals - 1].depth > c->depth) {
	i++;
	c->num_locals--;
    }
    
    if (c->depth == 0)
	i -= c->function->arity;
    
    emit_byte(S, OP_POP_N);
    emit_word(S, i); /* Pop everything except args */
}

void emit_byte(ParseState *S, u8 byte) {
    Function *function = S->current_compiler->function;
    function_write_byte(S->L, function, byte, S->previous.lineno);
}

void emit_word(ParseState *S, u16 word) {
    u8 upper, lower;
    upper = word >> 8;
    lower = word & 0x00ff;
    emit_byte(S, upper);
    emit_byte(S, lower);
}

void emit_constant(ParseState *S, Value constant) {
    Function *function = S->current_compiler->function;
    u16 addr = function_write_constant(S->L, function, constant);
    emit_word(S, addr);
}

size_t emit_jump(ParseState *S, Opcode type) {
    size_t where;

    emit_byte(S, type); /* Jump instruction */
    where = get_code_offset(S);
    emit_word(S, 0xFFFF); /* Placeholder jump offset */
    return where;
}

size_t get_code_offset(ParseState *S) {
    return S->current_compiler->function->code_size;
}

void emit_loop(ParseState *S, u16 where) {
    emit_byte(S, OP_LOOP);
    emit_word(S, get_code_offset(S) - where - 1);
}

#define CODE_AT(S, i) \
    (S->current_compiler->function->code[i])

void patch_jump(ParseState *S, size_t where) {
    u16 distance = get_code_offset(S) - where + 1;
    u8 upper = distance >> 8;
    u8 lower = distance & 0x00ff;
    CODE_AT(S, where) = upper;
    CODE_AT(S, where + 1) = lower;
}

#undef CODE_AT

void parse_program(ParseState *S) {
    advance(S);
    while (!match(S, TOK_EOF)) {
	parse_declaration(S);
    }
    emit_byte(S, OP_PUSH_NIL);
    emit_byte(S, OP_RETURN);
    emit_word(S, 0);
}

void parse_declaration(ParseState *S) {
    if (match(S, TOK_LET)) {
	parse_variable_declaration(S);
    } else if (match(S, TOK_FUNCTION)) {
	parse_function_declaration(S);
    } else {
	parse_statement(S);
    }
}

static b32 idcmp(Local l, Token t) {
    if (l.length != t.length)
	return false;
    return memcmp(l.name, t.start, l.length) == 0;
}

static void add_local(ParseState *S, Token identifier) {
    Compiler *c = S->current_compiler;
    Local local = {0};

    if (c->num_locals == 0xFFFF)
	error_previous(S, "too many local variables in function");

    local.name = identifier.start;
    local.length = identifier.length;
    local.depth = c->depth;
    c->locals[c->num_locals++] = local;
}

void declare_variable(ParseState *S) {
    int i;
    Token identifier = S->previous;
    Compiler *c = S->current_compiler;
    
    if (S->current_compiler->depth == 0)
	return;

    for (i = c->num_locals - 1; i >= 0; i--) {
	Local local = c->locals[i];

	if (local.depth != -1 && local.depth < c->depth)
	    break;
	if (idcmp(local, identifier))
	    error_previous(S,
			   "variable with name '%.*s' already declared in scope",
			   local.length, local.name);
    }
    add_local(S, identifier);
}

void define_variable(ParseState *S, Token identifier) {
    String *id = NULL;
    
    if (S->current_compiler->depth > 0)
	return;
    emit_byte(S, OP_DEFINE_GLOBAL);
    id = token_stringify(S, identifier);
    emit_constant(S, value_string(id));
}

int resolve_local(ParseState *S, Token identifier) {
    int i;
    Compiler *c = S->current_compiler;
    
    for (i = c->num_locals - 1; i >= 0; --i) {
	Local local = c->locals[i];
	
	if (local.depth > c->depth)
	    break;

	if (local.length == identifier.length) {
	    if (memcmp(local.name, identifier.start, local.length) == 0) {
		return i;
	    }
	}
    }

    return -1; /* Nothing found */
}

void parse_block(ParseState *S) {
    while (!peek(S, TOK_EOF) && !peek(S, TOK_END) && !peek(S, TOK_ELSE)) {
	parse_declaration(S);
    }
}

void parse_variable_declaration(ParseState *S) {
    Token identifier = {0};
    
    consume(S, TOK_IDENTIFIER, "expected identifier after 'let'");
    identifier = S->previous;
    declare_variable(S);
    if (match(S, TOK_EQUAL)) {
	parse_expression(S);
    } else {
	/* Emit nil */
    }
    define_variable(S, identifier);
    consume(S, TOK_SEMICOLON, "expected ';' after variable declaration");
}

/**
 * Parses function argument list. Sets arity for current compiler's function.
 */
static void parse_argument_list(ParseState *S) {
    int arity = 0;
    for (;;) {
	if (!match(S, TOK_IDENTIFIER))
	    break;
	arity++;
	declare_variable(S);
	if (!match(S, TOK_COMMA))
	    break;
    }
    S->current_compiler->function->arity = arity;
    consume(S, TOK_CLOSE_PAREN, "expected ')' after function parameters");
}

void parse_function_declaration(ParseState *S) {
    Compiler compiler = {0};
    Token identifier = {0};
    Function *function = NULL;

    consume(S, TOK_IDENTIFIER, "expected identifier after 'function'");
    identifier = S->previous;
    consume(S, TOK_OPEN_PAREN, "expected '('");
    begin_compiler(S, &compiler, token_stringify(S, identifier));
    begin_scope(S);
    parse_argument_list(S); /* Consumes ')' */
    consume(S, TOK_DO, "expected 'do' at beginning of function body");
    parse_block(S);
    consume(S, TOK_END, "expected 'end'");
    end_scope(S);
    function = end_compiler(S);
    emit_byte(S, OP_PUSH);
    emit_constant(S, value_function(function));
    define_variable(S, identifier);
}

void parse_statement(ParseState *S) {
    if (match(S, TOK_IF)) {
	parse_if_statement(S);
    } else if (match(S, TOK_WHILE)) {
	parse_while_statement(S);
    } else if (match(S, TOK_RETURN)) {
	parse_return_statement(S);
    } else if (match(S, TOK_PRINT)) {
	parse_print_statement(S);
    } else {
	parse_expression_statement(S);
    }
}

void parse_if_statement(ParseState *S) {
    size_t next, bottom;

    parse_expression(S);
    consume(S, TOK_THEN, "expected 'then'");
    begin_scope(S);
    next = emit_jump(S, OP_JUMP_IF_FALSE);
    parse_block(S);
    bottom = emit_jump(S, OP_JUMP);
    patch_jump(S, next);
    end_scope(S);
    if (match(S, TOK_ELSE)) {
        if (match(S, TOK_IF)) {
	    parse_if_statement(S);
	} else if (match(S, TOK_THEN)) {
	    parse_block(S);
	    consume(S, TOK_END, "expected 'end' after else block");
        } else {
	    parse_statement(S);
	}
    } else {
	consume(S, TOK_END, "expected 'end'");
    }
    patch_jump(S, bottom);
}

void parse_while_statement(ParseState *S) {
    u16 top;
    size_t bottom;

    top = get_code_offset(S);
    parse_expression(S);
    consume(S, TOK_DO, "expected 'do'");
    bottom = emit_jump(S, OP_JUMP_IF_FALSE);
    begin_scope(S);
    parse_block(S);
    consume(S, TOK_END, "expected 'end'");
    end_scope(S);
    emit_loop(S, top);
    patch_jump(S, bottom);
}

void parse_return_statement(ParseState *S) {
    if (S->current_compiler->enclosing == NULL)
	error_previous(S, "'return' is only permitted in functions");
    if (match(S, TOK_SEMICOLON)) {
	emit_byte(S, OP_PUSH_NIL);
    } else {
	parse_expression(S);
	consume(S, TOK_SEMICOLON, "expected ';' after return statement");
    }
    emit_byte(S, OP_RETURN);
    emit_word(S, S->current_compiler->function->arity);
}

void parse_expression_statement(ParseState *S) {
    parse_expression(S);
    consume(S, TOK_SEMICOLON, "expected ';' after expression");
    emit_byte(S, OP_POP);
}

void parse_print_statement(ParseState *S) {
    parse_expression(S);
    consume(S, TOK_SEMICOLON, "expected ';' after print statement");
    emit_byte(S, OP_PRINT);
}

/* Pratt parser stuff */

typedef enum {
    PREC_NONE,
    PREC_ASSIGNMENT,
    PREC_TERNARY,
    PREC_OR,
    PREC_AND,
    PREC_BITWISE_OR,
    PREC_BITWISE_AND,
    PREC_BITWISE_XOR,
    PREC_EQUALITY,
    PREC_COMPARISON,
    PREC_SHIFT,
    PREC_TERM,
    PREC_FACTOR,
    PREC_UNARY,
    PREC_CALL,
    PREC_SUBSCRIPT,
    PREC_PRIMARY
} Precedence;

typedef void (*ParseFn)(ParseState *S, b32 can_assign);

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence prec;
} ParseRule;

static void parse_precedence(ParseState *S, Precedence p);
static ParseRule dispatch_rule(ParseState *S, TokenType initial);

static ParseRule make_rule(ParseFn prefix, ParseFn infix, Precedence prec) {
    ParseRule rule;
    rule.prefix = prefix;
    rule.infix = infix;
    rule.prec = prec;
    return rule;
}

static void handle_grouping(ParseState *S, b32 can_assign) {
    parse_expression(S);
    consume(S, TOK_CLOSE_PAREN, "expected ')' after expression");
}

static u16 parse_map_table(ParseState *S) {
    u32 size = 0;

    while (!peek(S, TOK_CLOSE_BRACE)) {
	consume(S, TOK_OPEN_BRACKET, "expected '['");
	parse_expression(S);
	consume(S, TOK_CLOSE_BRACKET, "expected ']'");
	consume(S, TOK_EQUAL, "expected '='");
	parse_expression(S);
	size++;
	if (size > 0xFFFF)
	    error_previous(S, "cannot create table this large");
	if (!match(S, TOK_COMMA))
	    break;
    }

    return size;
}

static u16 parse_simple_table(ParseState *S) {
    u32 size = 0;

    if (peek(S, TOK_OPEN_BRACKET))
	return parse_map_table(S);
    
    while (!peek(S, TOK_CLOSE_BRACE)) {
	parse_precedence(S, PREC_TERNARY);
	size++;
	if (size > 0xFFFF)
	    error_previous(S, "cannot create table this large");
	if (!match(S, TOK_COMMA))
	    break;
    }

    return size;
}

static void handle_table(ParseState *S, b32 can_assign) {
    u16 size = 0;

    size = parse_simple_table(S);
    consume(S, TOK_CLOSE_BRACE, "expected '}' after table literal");
    emit_byte(S, OP_TABLE_MAKE);
    emit_word(S, (u16)size);
}

static void handle_unary(ParseState *S, b32 can_assign) {
    TokenType t = S->previous.type;
    parse_expression(S);
    switch (t) {
    case TOK_MINUS: emit_byte(S, OP_NEGATE); break;
    case TOK_TILDE: emit_byte(S, OP_BNOT); break;
    case TOK_BANG: emit_byte(S, OP_NOT); break;
    default: LUNA_ASSERT(0 && "unreachable");
    }
}

static void handle_binary(ParseState *S, b32 can_assign) {
    TokenType t = S->previous.type;
    ParseRule rule = dispatch_rule(S, t);
    parse_precedence(S, rule.prec - 1);
    switch (t) {
    case TOK_PLUS: emit_byte(S, OP_ADD); break;
    case TOK_MINUS: emit_byte(S, OP_SUBTRACT); break;
    case TOK_STAR: emit_byte(S, OP_MULTIPLY); break;
    case TOK_STAR_STAR: emit_byte(S, OP_POW); break;
    case TOK_SLASH: emit_byte(S, OP_DIVIDE); break;
    case TOK_GREATER: emit_byte(S, OP_GR); break;
    case TOK_GREATER_EQUAL: emit_byte(S, OP_GREQ); break;
    case TOK_GREATER_GREATER: emit_byte(S, OP_RSHIFT); break;
    case TOK_LESS: emit_byte(S, OP_LE); break;
    case TOK_LESS_EQUAL: emit_byte(S, OP_LEQ); break;
    case TOK_LESS_LESS: emit_byte(S, OP_LSHIFT); break;
    case TOK_EQUAL_EQUAL: emit_byte(S, OP_EQ); break;
    case TOK_BANG_EQUAL: emit_byte(S, OP_EQ); emit_byte(S, OP_NOT); break;
    case TOK_AMP: emit_byte(S, OP_BAND); break;
    case TOK_AMP_AMP: emit_byte(S, OP_AND); break;
    case TOK_PIPE: emit_byte(S, OP_BOR); break;
    case TOK_PIPE_PIPE: emit_byte(S, OP_OR); break;
    case TOK_CARAT: emit_byte(S, OP_BXOR); break;
    default: LUNA_ASSERT(0 && "unreachable");
    }
}

static void handle_literal(ParseState *S, b32 can_assign) {
    switch (S->previous.type) {
    case TOK_NIL: emit_byte(S, OP_PUSH_NIL); break;
    case TOK_TRUE: emit_byte(S, OP_PUSH_TRUE); break;
    case TOK_FALSE: emit_byte(S, OP_PUSH_FALSE); break;
    default: LUNA_ASSERT(0 && "unreachable"); break;
    }
}

static void handle_integer(ParseState *S, b32 can_assign) {
    int value = (int)strtol(S->previous.start, NULL, 10);
    emit_byte(S, OP_PUSH);
    emit_constant(S, value_int(value));
}

static void handle_double(ParseState *S, b32 can_assign) {
    double value = strtod(S->previous.start, NULL);
    emit_byte(S, OP_PUSH);
    emit_constant(S, value_double(value));
}

static void handle_string(ParseState *S, b32 can_assign) {
    size_t length = S->previous.length - 2; /* -2 to account for '"' */
    String *string = string_alloc_l(S->L, S->previous.start + 1, length);
    emit_byte(S, OP_PUSH);
    emit_constant(S, value_string(string));
}

static void handle_ternary(ParseState *S, b32 can_assign) {
    size_t first, second;

    first = emit_jump(S, OP_JUMP_IF_FALSE);
    parse_expression(S); /* True case */
    second = emit_jump(S, OP_JUMP);
    consume(S, TOK_COLON, "expected ':' in ternary");
    patch_jump(S, first);
    parse_expression(S); /* False case */
    patch_jump(S, second);
}

static void handle_subscript(ParseState *S, b32 can_assign) {
    parse_precedence(S, PREC_OR);
    consume(S, TOK_CLOSE_BRACKET, "expected ']'");
    if (can_assign && match(S, TOK_EQUAL)) {
	parse_expression(S);
	emit_byte(S, OP_TABLE_SET);
    } else {
	emit_byte(S, OP_TABLE_GET);
    }
}

static void handle_dot(ParseState *S, b32 can_assign) {
    String *identifier = NULL;
    
    consume(S, TOK_IDENTIFIER, "expected identifier for '.'");
    identifier = token_stringify(S, S->previous);
    emit_byte(S, OP_PUSH);
    emit_constant(S, value_string(identifier));
    if (can_assign && match(S, TOK_EQUAL)) {
	parse_expression(S);
	emit_byte(S, OP_TABLE_SET);
    } else {
        emit_byte(S, OP_TABLE_GET);
    }
}

static u16 parse_call_arglist(ParseState *S) {
    u16 arity = 0;
    while (!peek(S, TOK_CLOSE_PAREN)) {
	parse_expression(S);
	arity++;
	if (arity > 0xFF)
	    error_previous(S, "too many arguments to function");
	if (!match(S, TOK_COMMA))
	    break;
    }
    consume(S, TOK_CLOSE_PAREN, "expected ')' after argument list");
    return arity;
}

/**
 * Colon works like the dot operator but it is only for function calls. It will
 * implicitly place the table that it is being called on as the first argument
 * to the function call.
 */
static void handle_colon(ParseState *S, b32 can_assign) {
    String *identifier = NULL;
    u16 arity = 1;

    consume(S, TOK_IDENTIFIER, "expected identifier for ':'");
    identifier = token_stringify(S, S->previous);
    emit_byte(S, OP_PUSH);
    emit_constant(S, value_string(identifier));
    emit_byte(S, OP_TABLE_GET_WITH_SELF);
    consume(S, TOK_OPEN_PAREN, "expected '('");
    arity += parse_call_arglist(S);
    emit_byte(S, OP_CALL);
    emit_word(S, arity);
}

static void handle_variable(ParseState *S, b32 can_assign) {
    Token identifier = S->previous;
    int index = 0;
    Opcode set_op;
    Opcode get_op;

    index = resolve_local(S, identifier);
    if (index == -1) {
	set_op = OP_SET_GLOBAL;
	get_op = OP_GET_GLOBAL;
    } else {
	set_op = OP_SET_LOCAL;
	get_op = OP_GET_LOCAL;
    }
    
    if (can_assign && match(S, TOK_EQUAL)) {
	parse_expression(S);
	emit_byte(S, set_op);
    } else {
	emit_byte(S, get_op);
    }

    if (index == -1)
	emit_constant(S, value_string(token_stringify(S, identifier)));
    else
	emit_word(S, index);
}

static void handle_call(ParseState *S, b32 can_assign) {
    u16 arity = parse_call_arglist(S);
    emit_byte(S, OP_CALL);
    emit_word(S, arity);
}

ParseRule dispatch_rule(ParseState *S, TokenType initial) {
    switch (initial) {
    case TOK_OPEN_PAREN: return make_rule(handle_grouping, handle_call, PREC_CALL);
    case TOK_OPEN_BRACE: return make_rule(handle_table, NULL, PREC_NONE);
    case TOK_MINUS: return make_rule(handle_unary, handle_binary, PREC_TERM);
    case TOK_TILDE: return make_rule(handle_unary, NULL, PREC_NONE);
    case TOK_BANG: return make_rule(handle_unary, NULL, PREC_NONE);
    case TOK_PIPE_PIPE: return make_rule(NULL, handle_binary, PREC_OR);
    case TOK_AMP_AMP: return make_rule(NULL, handle_binary, PREC_AND);
    case TOK_PIPE: return make_rule(NULL, handle_binary, PREC_BITWISE_OR);
    case TOK_AMP: return make_rule(NULL, handle_binary, PREC_BITWISE_AND);
    case TOK_CARAT: return make_rule(NULL, handle_binary, PREC_BITWISE_XOR);
    case TOK_EQUAL_EQUAL:
    case TOK_BANG_EQUAL: return make_rule(NULL, handle_binary, PREC_EQUALITY);
    case TOK_GREATER:
    case TOK_GREATER_EQUAL:
    case TOK_LESS:
    case TOK_LESS_EQUAL: return make_rule(NULL, handle_binary, PREC_COMPARISON);
    case TOK_GREATER_GREATER:
    case TOK_LESS_LESS: return make_rule(NULL, handle_binary, PREC_SHIFT);
    case TOK_PLUS: return make_rule(NULL, handle_binary, PREC_TERM);
    case TOK_SLASH: return make_rule(NULL, handle_binary, PREC_FACTOR);
    case TOK_STAR: return make_rule(NULL, handle_binary, PREC_FACTOR);
    case TOK_NIL:
    case TOK_TRUE:
    case TOK_FALSE: return make_rule(handle_literal, NULL, PREC_NONE);
    case TOK_INTEGER: return make_rule(handle_integer, NULL, PREC_NONE);
    case TOK_DOUBLE: return make_rule(handle_double, NULL, PREC_NONE);
    case TOK_STRING: return make_rule(handle_string, NULL, PREC_NONE);
    case TOK_QUESTION: return make_rule(NULL, handle_ternary, PREC_TERNARY);
    case TOK_OPEN_BRACKET: return make_rule(
	NULL, handle_subscript, PREC_SUBSCRIPT);
    case TOK_DOT: return make_rule(NULL, handle_dot, PREC_SUBSCRIPT);
    case TOK_COLON: return make_rule(NULL, handle_colon, PREC_CALL);
    case TOK_IDENTIFIER: return make_rule(handle_variable, NULL, PREC_NONE);
    default: return make_rule(NULL, NULL, PREC_NONE);
    }
}

void parse_precedence(ParseState *S, Precedence p) {
    ParseFn prefix = NULL;
    b32 can_assign = false;

    advance(S);
    prefix = dispatch_rule(S, S->previous.type).prefix;
    if (!prefix) {
	error_previous(S, "expected expression");
    }

    can_assign = p <= PREC_ASSIGNMENT;
    prefix(S, can_assign);
    while (p <= dispatch_rule(S, S->current.type).prec) {
	ParseFn infix = NULL;

	advance(S);
	infix = dispatch_rule(S, S->previous.type).infix;
	LUNA_ASSERT(infix != NULL);
	infix(S, can_assign);
    }
}

void parse_expression(ParseState *S) {
    parse_precedence(S, PREC_ASSIGNMENT);
}
