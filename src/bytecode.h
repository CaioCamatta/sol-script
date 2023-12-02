#ifndef delta_bytecode_h
#define delta_bytecode_h

typedef enum {
    OP_CONSTANT,
    OP_ADD,
    OP_PRINT
} Opcode;

typedef struct {
    double constant;  // TODO, store this in constants table
} BytecodeConstant;

// ## suppresses substitution
#define BYTECODE_CONSTANT(value)                                \
    (Bytecode) {                                                \
        .type = OP_CONSTANT, .operands = {.constant = {value} } \
    }

typedef struct {
} BytecodeAdd;

#define BYTECODE_ADD() \
    (Bytecode) { .type = OP_ADD }

/* The bytecode contains the Opcode and optional operands depending on the type of operation. */
typedef struct {
    Opcode type;
    union {
        BytecodeConstant constant;
        BytecodeAdd add;
    } operands;
} Bytecode;

#endif