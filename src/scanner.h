#ifndef delta_scanner_h
#define delta_scanner_h

#include "array.h"
#include "token.h"

/**
 * Scanner struct to facilitate scanning through a file.
 *
 * @param current Current character. 0-indexed.
 * @param start Start of the current lexeme, inclusive.
 * @param lineNo Current line, for error reporting. 1-indexed.
 * @param colNo Current column, for error reporting. 1-indexed.
 * */
typedef struct {
    const char* current;
    const char* start;
    int lineNo;
    int colNo;
} Scanner;

/**
 * Turn source code into an array of tokens.
 *
 * Throws exceptions in case of lexical errors.
 *
 * @param scanner an initialized Scanner to use for scanning.
 */
TokenArray scanTokens(Scanner* scanner);

/**
 * Reset the scanner and turn source code into an array of tokens.
 *
 * Throws exceptions in case of lexical errors.
 *
 * @param sourceCode the source code to scan.
 */
TokenArray scanTokensFromString(Scanner* scanner, const char* sourceCode);

/**
 * Scan the next Token following the Delta lexical Grammar.
 *
 * Throws exceptions in case of lexical errors.
 *
 * @param scanner an initialized Scanner to use for scanning.
 */
Token scanNext(Scanner* scanner);

/* Initialize scanner at the beginning of the given sourceCode to be scanned. */
void initScanner(Scanner* scanner, const char* sourceCode);

#endif