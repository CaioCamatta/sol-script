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

#endif