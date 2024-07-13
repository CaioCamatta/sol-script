#ifndef sol_script_value_h
#define sol_script_value_h

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#include "bytecode.h"

/**
 * The stack holds Values. A Value represents any SolScript type like booleans, numbers, strings, etc.
 *
 * TODO: one of the highest leverage optimizations for SolScript is to use NaN boxing instead of tagged union.
 */
typedef enum {
    TYPE_DOUBLE,
    TYPE_STRING,
    TYPE_BOOLEAN,
    TYPE_NULL,
    TYPE_LAMBDA
} ValueType;
typedef struct {
    ValueType type;

    union {
        double doubleVal;
        const char* stringVal;  // This string should live in the constant pool, which persists through the VM's lifetime.
        bool booleanVal;
        void* lambdaVal;  // Runtime function objects in SolScript are just a pointer to the code object.
    } as;
} Value;

#define DOUBLE_VAL(doubleValArg)                                \
    (Value) {                                                   \
        .type = TYPE_DOUBLE, .as = {.doubleVal = doubleValArg } \
    }

#define NULL_VAL()        \
    (Value) {             \
        .type = TYPE_NULL \
    }

#define BOOL_VAL(boolValArg)                                    \
    (Value) {                                                   \
        .type = TYPE_BOOLEAN, .as = {.booleanVal = boolValArg } \
    }

#define STRING_VAL(stringValArg)                                \
    (Value) {                                                   \
        .type = TYPE_STRING, .as = {.stringVal = stringValArg } \
    }

#define LAMBDA_VAL(functionPtr)                                \
    (Value) {                                                  \
        .type = TYPE_LAMBDA, .as = {.lambdaVal = functionPtr } \
    }

#define IS_DOUBLE(value) ((value).type == TYPE_DOUBLE)
#define IS_NULL(value) ((value).type == TYPE_NULL)
#define IS_BOOLEAN(value) ((value).type == TYPE_BOOLEAN)
#define IS_STRING(value) ((value).type == TYPE_STRING)
#define IS_LAMBDA(value) ((value).type == TYPE_LAMBDA)

#endif