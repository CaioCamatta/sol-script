#include "error.h"

#include <stdio.h>
#include <stdlib.h>

#include "util/array.h"
#include "util/colors.h"

void initErrorArray(ErrorArray* errorArray) {
    INIT_ARRAY((*errorArray), Error);
}

void addError(ErrorArray* errorArray, const char* message, Token token) {
    Error error = {
        .message = message,
        .token = token};
    INSERT_ARRAY((*errorArray), error, Error);
}

void printErrors(ErrorArray* errorArray) {
    for (size_t i = 0; i < errorArray->used; i++) {
        Error* error = &errorArray->values[i];
        fprintf(stderr, KRED "Error" RESET " [line %d, column %d]: %s\n",
                error->token.lineNo, error->token.colNo, error->message);
    }
}

bool hadError(ErrorArray* errorArray) {
    return errorArray->used > 0;
}