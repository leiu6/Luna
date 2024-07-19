#include <stdio.h>
#include <stdlib.h>

#include "defs.h"
#include "state.h"

NORETURN void launch_repl(void);

NORETURN void load_script(const char *path);

NORETURN void print_usage(const char *program);

int main(int argc, char **argv) {
    switch (argc) {
    case 1:
	launch_repl();

    case 2:
	load_script(argv[1]);

    default:
	print_usage(argv[0]);
    }
}

void launch_repl(void) {
    Luna_State *L = luna_state_new();

    printf("Welcome to the Luna interactive REPL!\n");
    for (;;) {
	char line[1024];
	printf("> ");
	fgets(line, 1024, stdin);
        (void)luna_execute_string(L, line);
    }
    
    luna_state_delete(L);
}

void load_script(const char *path) {
    FILE *f = NULL;
    size_t size = 0;
    size_t size_read = 0;
    char *buffer = NULL;
    Luna_State *L = NULL;

    f = fopen(path, "rb");
    if (!f)
	goto exit_failure;

    fseek(f, 0, SEEK_END);
    size = ftell(f);
    rewind(f);
    buffer = calloc(1, size + 1);
    if (!buffer)
	goto exit_failure;
    
    size_read = fread(buffer, 1, size, f);
    if (size_read < size)
	goto exit_failure;

    L = luna_state_new();
    if (!L)
	goto exit_failure;
    exit(luna_execute_string(L, buffer));

exit_failure:
    fprintf(stderr, "failed to load script '%s'", path);
    
    if (f)
	fclose(f);

    if (buffer)
	free(buffer);

    if (L)
	luna_state_delete(L);

    exit(1);
}

void print_usage(const char *program) {
    fprintf(stderr, "Usage: %s [script] or just %s", program, program);
    exit(1);
}
