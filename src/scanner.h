#ifndef delta_scanner_h
#define delta_scanner_h

#include "token.h"

/**
 * Turn source code into an array of tokens.
 *
 * Throws exceptions in case of lexical errors.
 *
 * @param inputCode a string of characters to be parsed.
 */
TokenType* scan(char* inputCode);

/**
 * Scan the next from a given index.
 *
 * Throws exceptions in case of lexical errors.
 *
 * @param inputCode a string of characters to be parsed.
 * @param cursorIndex the index (inclusive) to start scanning from.
 */
TokenType scanNext(char* inputCode, int cursorIndex);

/* Scanner struct to facilitate scanning through a file. */
typedef struct {
    const char* current;  // Current character. 0-indexed.
    const char* start;    // Start of the current lexeme, inclusive.
    int line;             // Current line, for error reporting. 1-indexed.
    int charPos;          // The position of the curent character relative to the start of
                          // the current line, for error reporting. 1-indexed.
} Scanner;

/* Initialize scanner at the beginning of the given sourceCode */
void initScanner(Scanner* scanner, const char* sourceCode);

#endif