#ifndef delta_bytecode_h
#define delta_bytecode_h

#include <stddef.h>

#include "value.h"

typedef enum {
    OP_CONSTANT,
    OP_ADD,
    OP_PRINT
} Opcode;

typedef struct {
    ValueType type;
    union {
        double doubleVal;  // TODO, store this in constants table
    } as;
} BytecodeConstant;

// ## suppresses substitution
#define BYTECODE_CONSTANT_DOUBLE(value)                                        \
    (Bytecode) {                                                               \
        .type = OP_CONSTANT,                                                   \
        .operands = {                                                          \
            .constant = (BytecodeConstant){.type = TYPE_DOUBLE, .as = {value}} \
        }                                                                      \
    }

typedef struct {
} BytecodeAdd;

#define BYTECODE(op) \
    (Bytecode) { .type = op }

/* The bytecode contains the Opcode and optional operands depending on the type of operation. */
typedef struct {
    Opcode type;
    union {
        BytecodeConstant constant;
        BytecodeAdd add;
    } operands;
} Bytecode;

typedef struct {
    Bytecode *values;
    size_t used;
    size_t size;
} BytecodeArray;

#endif