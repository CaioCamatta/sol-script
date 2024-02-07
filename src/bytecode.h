#ifndef delta_bytecode_h
#define delta_bytecode_h

#include <stddef.h>

#include "value.h"

// --------------------------------- Bytecode ----------------------------------
typedef enum {
    OP_LOAD_CONSTANT,  // load a constant from the compiled constant pool onto the stack
    OP_SET_VAL,        // expects an identifier at the top of the stack, and a value right below it
    OP_GET_VAL,        // expects an identifier at the top of the stack
    OP_TRUE,           // put Value true on the stack
    OP_FALSE,          // put Value false on the stack
    OP_ADD,            // add two numbers at the top of the stack, replace them with the result Value
    OP_PRINT           // print value at the top of the stack
} Opcode;

// Create simple bytecode with no operands or constants
#define BYTECODE(op) \
    (Bytecode) { .type = op }

// Create bytecode with one constant
#define BYTECODE_CONSTANT_1(op, index1) \
    (Bytecode) {                        \
        .type = op,                     \
        .maybeConstantIndex = (index1)  \
    }

/* The bytecode contains the Opcode and optional operands depending on the type of operation. */
typedef struct {
    Opcode type;
    size_t maybeConstantIndex;
} Bytecode;

typedef struct {
    Bytecode* values;
    size_t used;
    size_t size;
} BytecodeArray;

// --------------------------------- Constant Pool ----------------------------------

typedef enum {
    CONST_TYPE_STRING,
    CONST_TYPE_DOUBLE  // TODO: stop putting doubles in constant pool; inline it in the bytecode
} ConstantType;

typedef struct {
    ConstantType type;
    union {
        char* string;  // Null-terminated
        double number;
    } as;
} Constant;

#define STRING_CONST(nullTerminatedStringArg)      \
    (Constant) {                                   \
        .type = CONST_TYPE_STRING,                 \
        .as = {.string = nullTerminatedStringArg}, \
    }

#define DOUBLE_CONST(numberArg)      \
    (Constant) {                     \
        .type = CONST_TYPE_DOUBLE,   \
        .as = {.number = numberArg}, \
    }

/**
 * Pool to store Values so they can be referenced at runtime.
 *
 * This exists because we need a way to pass literals (e.g. strings, doubles, etc) to the
 * VM. Including a string in the bytecode would be a mess and result in arbitrarily long
 * bytecode.
 */
typedef struct {
    Constant* values;
    size_t used;
    size_t size;
} ConstantPool;

// --------------------------------- Compiled Code ----------------------------------

/**
 * Delta's compiled code consists of a constant pool and the bytecode.
 * This is all the information the VM needs to run.
 */
typedef struct {
    ConstantPool constantPool;
    BytecodeArray bytecodeArray;
} CompiledCode;

#endif