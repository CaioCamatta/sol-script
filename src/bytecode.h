#ifndef sol_script_bytecode_h
#define sol_script_bytecode_h

#include <stddef.h>

#include "value.h"

// --------------------------------- Bytecode ----------------------------------
/**
 * Note: I decided to explode the binary operations into one bytecode per operation (like in the JVM)
 * instead of one general OP_BINARY + operands to keep the opcode semantics clear.
 * TODO: benchmark OP_BINARY(op) instead of OP_BINARY*
 */
typedef enum {
    OP_LOAD_CONSTANT,       // load a constant from the compiled constant pool onto the stack
    OP_SET_GLOBAL_VAL,      // expects an identifier at the top of the stack, and a value right below it
    OP_GET_GLOBAL_VAL,      // expects an identifier at the top of the stack
    OP_SET_LOCAL_VAL_FAST,  // turn the value at the top of the stack into a local variable.
    OP_GET_LOCAL_VAL_FAST,  // load a local variable that's already in the stack
    OP_TRUE,                // put Value true on the stack
    OP_FALSE,               // put Value false on the stack
    OP_PRINT,               // print value at the top of the stack
    OP_POPN,                // pop N values from the stack
    OP_JUMP_IF_FALSE,       // pop the value on top of the stack, and jump it's falsy
    OP_JUMP,                // jump unconditionally

    // Unary operations
    OP_UNARY_NEGATE,  // -stack[-1]
    OP_UNARY_NOT,     // !stack[-1]

    // Binary operations.
    OP_BINARY_ADD,          // stack[-2] + stack[-1]
    OP_BINARY_SUBTRACT,     // stack[-2] - stack[-1]
    OP_BINARY_MULTIPLY,     // stack[-2] * stack[-1]
    OP_BINARY_DIVIDE,       // stack[-2] / STACK[-1]
    OP_BINARY_GT,           // stack[-2] > STACK[-1]
    OP_BINARY_GTE,          // stack[-2] >= STACK[-1]
    OP_BINARY_LT,           // stack[-2] < STACK[-1]
    OP_BINARY_LTE,          // stack[-2] <= STACK[-1]
    OP_BINARY_LOGICAL_AND,  // stack[-2] && STACK[-1]
    OP_BINARY_LOGICAL_OR,   // stack[-2] || STACK[-1]
    OP_BINARY_EQUAL,        // stack[-2] == STACK[-1]
    OP_BINARY_NOT_EQUAL     // stack[-2] != STACK[-1]
} Opcode;

// Create simple bytecode with no operands or constants
#define BYTECODE(op) \
    (Bytecode) { .type = op }

// Create bytecode with one constant
#define BYTECODE_OPERAND_1(op, index1) \
    (Bytecode) {                       \
        .type = op,                    \
        .maybeOperand1 = (index1)      \
    }

/* The bytecode contains the Opcode and optional operands depending on the type of operation. */
typedef struct {
    Opcode type;
    size_t maybeOperand1;
} Bytecode;

typedef struct {
    Bytecode* values;
    size_t used;
    size_t size;
} BytecodeArray;

// --------------------------------- Constant Pool ----------------------------------

typedef enum {
    CONST_TYPE_STRING,
    CONST_TYPE_IDENTIFIER,  // identifiers are similar to strings, but handled slightly different
    CONST_TYPE_DOUBLE       // TODO: stop putting doubles in constant pool; inline it in the bytecode
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

// Global variable name with end up as identifiers in the constant pool.
#define IDENTIFIER_CONST(nullTerminatedIdentifierArg)  \
    (Constant) {                                       \
        .type = CONST_TYPE_IDENTIFIER,                 \
        .as = {.string = nullTerminatedIdentifierArg}, \
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
 * SolScript's compiled code consists of a constant pool and the bytecode.
 * This is all the information the VM needs to run.
 */
typedef struct {
    ConstantPool constantPool;
    BytecodeArray bytecodeArray;
} CompiledCode;

#endif