#include "error.h"

#include <stdio.h>
#include <stdlib.h>

#include "util/colors.h"

void initErrorArray(ErrorArray* errorArray) {
    INIT_ARRAY((*errorArray), Error);
}

void addError(ErrorArray* errorArray, const char* message, Token token, ErrorType type) {
    Error error = {
        .message = message,
        .token = token,
        .type = type};
    INSERT_ARRAY((*errorArray), error, Error);
}

static char const* tokenTypeStrings[] = {
    [SCANNER_ERROR] = "ScannerError",
    [PARSER_ERROR] = "ParserError",
    [COMPILER_ERROR] = "CompilerError",
};

void printErrorMessage(Error* error) {
    const char* errorType = tokenTypeStrings[error->type];

    fprintf(stderr, KRED "%s" RESET KGRY "[%d:%d]" RESET, errorType, error->token.lineNo, error->token.colNo);

    if (error->token.type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (error->token.type != TOKEN_ERROR) {
        fprintf(stderr, " at '%.*s'", error->token.length, error->token.start);
    }

    fprintf(stderr, ": %s\n", error->message);
}

/**
 * Using the Token in the Error, print the line of source code where the error
 * occurred, plus a carat indicating the location of the error.
 *
 * This function is rudimentary and not super efficient, but that's ok because
 * errors are not abundant and fast compilation speed is not one of SolScript's goals.
 */
static void printSourceLine(Error* error) {
    // Find the beginning of the line
    const char* lineStart = error->token.start;
    while (lineStart > (error->token.start - error->token.colNo + error->token.length + 1) && lineStart[-1] != '\n') {
        lineStart--;
    }

    // Find the end of the line
    const char* lineEnd = error->token.start;
    while (*lineEnd != '\0' && *lineEnd != '\n' && *lineEnd != EOF) {
        lineEnd++;
    }

    // Print the line
    int lineLength = lineEnd - lineStart;
    if (lineLength > 0) {
        fprintf(stderr, "    %.*s\n", lineLength, lineStart);
    } else {
        fprintf(stderr, "    <empty line>\n");
    }

    // Print the caret
    for (int i = 0; i < (error->token.start - lineStart) + 4; i++) {
        fprintf(stderr, " ");
    }
    fprintf(stderr, "^\n");
}

void printErrors(ErrorArray* errorArray) {
    for (size_t i = 0; i < errorArray->used; i++) {
        Error* error = &errorArray->values[i];
        printErrorMessage(error);
        printSourceLine(error);
        fprintf(stderr, "\n");
    }
}

bool hadError(ErrorArray* errorArray) {
    return errorArray->used > 0;
}