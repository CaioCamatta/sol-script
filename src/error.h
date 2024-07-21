#ifndef sol_script_error_h
#define sol_script_error_h

#include <stdbool.h>

#include "token.h"
#include "util/array.h"

typedef enum {
    SCANNER_ERROR,
    PARSER_ERROR,
    COMPILER_ERROR,
} ErrorType;

/**
 * Struct to represent a compile-time error, which could come from the
 * scanner, parser, or compiler.
 *
 * The Token contains the location (line, column) of the error.
 */
typedef struct {
    const char* message;
    Token token;
    ErrorType type;
} Error;

/**
 * An array of compile-time errors. This struct can be used by the
 * scanner, parser, or compiler, but it shouldn't be shared across them.
 */
typedef struct {
    Error* values;
    size_t used;
    size_t size;
} ErrorArray;

void initErrorArray(ErrorArray* errorArray);
void addError(ErrorArray* errorArray, const char* message, Token token, ErrorType type);
void printErrors(ErrorArray* errorArray);
void freeErrorArray(ErrorArray* errorArray);
bool hadError(ErrorArray* errorArray);

#endif