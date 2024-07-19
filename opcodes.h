#ifndef LUNA_OPCODES_H
#define LUNA_OPCODES_H

/**
 * Opcodes are defined by an X macro to allow them to be easily pretty printed
 * and for other debugging purposes. Opcodes that take an argument should be
 * added to the list with the X_ARG(name) macro. This will define the proper
 * enum and allow the argument to be displayed during function disassembly. For
 * standard entries, just use the X(name) macro.
 */
#define OPCODES					\
    X(OP_ADD),					\
    X(OP_SUBTRACT),				\
    X(OP_MULTIPLY),				\
    X(OP_DIVIDE),				\
	X(OP_POW),				\
	X(OP_GR),				\
	X(OP_GREQ),				\
	X(OP_LE),				\
	X(OP_LEQ),				\
	X(OP_EQ),				\
	X(OP_NOT),				\
	X(OP_BNOT),				\
	X(OP_AND),				\
	X(OP_BAND),				\
	X(OP_OR),				\
	X(OP_BOR),				\
	X(OP_NEGATE),				\
	X(OP_LSHIFT),				\
	X(OP_RSHIFT),				\
	X(OP_BXOR),				\
						\
	X_ARG(OP_PUSH),				\
	X(OP_PUSH_NIL),			\
	X(OP_PUSH_TRUE),			\
	X(OP_PUSH_FALSE),			\
	X(OP_POP),				\
	X_ARG(OP_POP_N),\
	\
	X(OP_PRINT),\
	\
	X_ARG(OP_JUMP),\
	X_ARG(OP_JUMP_IF_FALSE),\
	X_ARG(OP_LOOP),\
						\
	X_ARG(OP_DEFINE_GLOBAL),		\
	X_ARG(OP_SET_GLOBAL),			\
	X_ARG(OP_GET_GLOBAL),			\
	X_ARG(OP_SET_LOCAL),			\
	X_ARG(OP_GET_LOCAL),			\
	\
	X(OP_TABLE_SET),\
	X(OP_TABLE_GET),\
	X(OP_TABLE_GET_WITH_SELF),\
	X_ARG(OP_TABLE_MAKE),\
						\
	X_ARG(OP_CALL),				\
	X_ARG(OP_RETURN)

#define X(op) op
#define X_ARG(op) op
typedef enum {OPCODES} Opcode;
#undef X
#undef X_ARG

#endif /* LUNA_OPCODES_H */
