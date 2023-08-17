#include "scanner.h"

#include <ctype.h>
#include <stdbool.h>

#include "config.h"

void initScanner(Scanner* scanner, const char* sourceCode) {
    scanner->start = sourceCode;
    scanner->current = sourceCode;
    scanner->line = 1;
    scanner->charPos = 1;
}

static bool isAlpha(char c) {
    return isalpha(c) || c == '_';
}

static bool isDigit(char c) {
    return isdigit(c);
}

static bool isAtEnd(Scanner* scanner) {
    return scanner->current == '\0';
}

/* Return current char and advance pointer */
static char advance(Scanner* scanner) {
    scanner->current++;
    scanner->charPos++;
    return scanner->current[-1];
}

/* Peek current char without advancing pointer */
static char peek(Scanner* scanner) {
    return scanner->current;
}

/* Peek next char without advancing (lookahead 1) */
static char peekNext(Scanner* scanner) {
    if (isAtEnd(scanner)) return '\0';
    return scanner->current[1];
}

TokenType* scan(char* sourceCode) {
    Scanner scanner;
    return scanNext(sourceCode, 0);
}

TokenType scanNext(char* sourceCode, int cursorIndex) {
    return TOKEN_VAL;
}