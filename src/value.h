#ifndef delta_value_h
#define delta_value_h

/**
 * The stack holds Values. A Value represents any Delta type like booleans, numbers, strings, etc.
 *
 * TODO: one of the highest leverage optimizations for Delta is to use NaN boxing instead of tagged union.
 */
typedef enum {
    TYPE_DOUBLE,
    TYPE_STRING,
    TYPE_BOOLEAN,
    TYPE_NULL
} ValueType;
typedef struct {
    ValueType type;

    union {
        double doubleVal;
    } as;
} Value;

#define DOUBLE_VAL(doubleValArg)                                \
    (Value) {                                                   \
        .type = TYPE_DOUBLE, .as = {.doubleVal = doubleValArg } \
    }

#endif