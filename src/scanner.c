#include "scanner.h"

TokenType scan(char* inputCode) {
    return scanNext(inputCode, 0);
}

TokenType scanNext(char* inputCode, int cursorIndex) {
    return TOKEN_VAL;
}