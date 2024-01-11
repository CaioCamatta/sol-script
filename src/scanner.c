#include "scanner.h"

#include <ctype.h>
#include <stdbool.h>
#include <string.h>

#include "array.h"
#include "config.h"

void initScanner(Scanner* scanner, const char* sourceCode) {
    scanner->start = sourceCode;
    scanner->current = sourceCode;
    scanner->lineNo = 1;
    scanner->colNo = 1;
}

static bool isAlpha(char c) {
    return isalpha(c) || c == '_';
}

static bool isDigit(char c) {
    return isdigit(c);
}

static bool isAtEnd(Scanner* scanner) {
    return *(scanner->current) == '\0';
}

/* Return current char and advance pointer */
static char advance(Scanner* scanner) {
    scanner->current++;
    scanner->colNo++;
    return scanner->current[-1];
}

/* Peek current char without advancing pointer */
static char peek(Scanner* scanner) {
    return *(scanner->current);
}

/* Peek next char without advancing (lookahead 1) */
static char peekNext(Scanner* scanner) {
    if (isAtEnd(scanner)) return '\0';
    return scanner->current[1];
}

/* Advance if next char matches expected. Returns true if the scanner was advanced, and false otherwise. */
static bool advanceIfMatch(Scanner* scanner, char expected) {
    if (isAtEnd(scanner)) return false;
    if (peek(scanner) != expected) return false;
    advance(scanner);
    return true;
}

static Token makeToken(Scanner* scanner, TokenType type) {
    Token token = {
        .type = type,
        .start = scanner->start,
        .length = (int)(scanner->current - scanner->start),
        .lineNo = scanner->lineNo,
        .colNo = scanner->colNo};

    return token;
}

static Token errorToken(Scanner* scanner, const char* message) {
    Token token = {
        .type = TOKEN_ERROR,
        .start = token.start = message,  // point to message instead of user's source code,
        .length = (int)strlen(message),
        .lineNo = scanner->lineNo,
        .colNo = scanner->colNo};

    return token;
}

static void skipWhitespace(Scanner* scanner) {
    for (;;) {
        char c = peek(scanner);
        switch (c) {
            case ' ':
            case '\r':
            case '\t':
                advance(scanner);
                break;
            case '\n':
                scanner->lineNo++;
                scanner->colNo = 1;
                advance(scanner);
                break;
            case '/':
                if (peekNext(scanner) == '/') {
                    // A comment goes until the end of the line.
                    while (peek(scanner) != '\n' && !isAtEnd(scanner)) advance(scanner);
                } else {
                    // don't consume first slash if its not a comment
                    return;
                }
                break;
            default:
                return;
        }
    }
}

/*
 * Create a Token of 'tokenType' if 'keyword' matches current string in the 'scanner'.
 * Otherwise do nothing.
 * TODO: optimize 'strlen(keyword)'; ideally it should be calculated at compile time
 */
#define matchKeyword(scanner, keyword, tokenType)               \
    do {                                                        \
        int keywordLen = strlen(keyword);                       \
        if (scanner->current - scanner->start == keywordLen &&  \
            memcmp(scanner->start, keyword, keywordLen) == 0) { \
            return makeToken(scanner, tokenType);               \
        }                                                       \
    } while (0)

/*
 * Match either a keyword or identifier and create token with it.
 */
static Token identifierOrKeyword(Scanner* scanner) {
    // Scan the whole work
    while (isAlpha(peek(scanner)) || isDigit(peek(scanner))) advance(scanner);

    // Once the whole word is scanned, match it with a keyword or default to an identifier
    switch (scanner->start[0]) {
        case 'e':
            matchKeyword(scanner, "else", TOKEN_ELSE);
        case 'f':
            matchKeyword(scanner, "false", TOKEN_FALSE);
        case 'i':
            matchKeyword(scanner, "if", TOKEN_IF);
        case 'n':
            matchKeyword(scanner, "null", TOKEN_NULL);
        case 'v':
            matchKeyword(scanner, "val", TOKEN_VAL);
        case 'r':
            matchKeyword(scanner, "return", TOKEN_RETURN);
        case 't':
            matchKeyword(scanner, "true", TOKEN_TRUE);
        case 's':
            matchKeyword(scanner, "struct", TOKEN_STRUCT);
        default:
            return makeToken(scanner, TOKEN_IDENTIFIER);
    }
}

/* Grammar rule for number literals */
static Token number(Scanner* scanner) {
    while (isDigit(peek(scanner))) advance(scanner);

    // Fractional part
    if (peek(scanner) == '.' && isDigit(peekNext(scanner))) {
        // Consume the '.'
        advance(scanner);

        while (isDigit(peek(scanner))) advance(scanner);
    }

    return makeToken(scanner, TOKEN_NUMBER);
}

/* Grammar rule for string literals */
static Token string(Scanner* scanner) {
    while (peek(scanner) != '"' && !isAtEnd(scanner)) {
        if (peek(scanner) == '\n') scanner->lineNo++;
        advance(scanner);
    }
    if (isAtEnd(scanner)) return errorToken(scanner, "Unterminated string.");

    advance(scanner);
    // Note: we defer converting the litera 2l lexeme to a runtime value later, in the compiter.
    return makeToken(scanner, TOKEN_STRING);
}

Token scanNext(Scanner* scanner) {
    skipWhitespace(scanner);
    scanner->start = scanner->current;

    if (isAtEnd(scanner)) return makeToken(scanner, TOKEN_EOF);

    char c = advance(scanner);

    // Identifier or keyword
    if (isAlpha(c)) return identifierOrKeyword(scanner);

    // Number literal
    if (isDigit(c)) return number(scanner);

    // String literal
    if (c == '"') return string(scanner);

    // Punctuators
    switch (c) {
        case '(':
            return makeToken(scanner, TOKEN_LEFT_PAREN);
        case ')':
            return makeToken(scanner, TOKEN_RIGHT_PAREN);
        case '{':
            return makeToken(scanner, TOKEN_LEFT_CURLY);
        case '}':
            return makeToken(scanner, TOKEN_RIGHT_CURLY);
        case '.':
            return makeToken(scanner, TOKEN_DOT);
        case '*':
            return makeToken(scanner, TOKEN_STAR);
        case '/':
            return makeToken(scanner, TOKEN_SLASH);
        case '+':
            return makeToken(scanner, TOKEN_PLUS);
        case '-':
            return makeToken(scanner, TOKEN_MINUS);
        case '!':
            return makeToken(scanner,
                             advanceIfMatch(scanner, '=') ? TOKEN_EXCLAMATION_EQUAL : TOKEN_EXCLAMATION);
        case '%':
            return makeToken(scanner, TOKEN_MODULO);
        case '<':
            return makeToken(scanner,
                             advanceIfMatch(scanner, '=') ? TOKEN_LESSER_EQUAL : TOKEN_LESSER);
        case '>':
            return makeToken(scanner,
                             advanceIfMatch(scanner, '=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER);
        case '=':
            return makeToken(scanner,
                             advanceIfMatch(scanner, '=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL);
        case '|':
            if (advanceIfMatch(scanner, '|'))
                return makeToken(scanner, TOKEN_OR_OR);
        case '&':
            if (advanceIfMatch(scanner, '&'))
                return makeToken(scanner, TOKEN_AND_AND);
        case ';':
            return makeToken(scanner, TOKEN_SEMICOLON);
        case ',':
            return makeToken(scanner, TOKEN_COMMA);
    }

    return errorToken(scanner, "Unexpected character.");
}

TokenArray scanTokens(Scanner* scanner) {
    TokenArray tokens;
    INIT_ARRAY(tokens, Token);

    Token token;
    do {
        token = scanNext(scanner);
        INSERT_ARRAY(tokens, token, Token);
    } while (token.type != TOKEN_EOF);

    return tokens;
}

TokenArray scanTokensFromString(Scanner* scanner, const char* sourceCode) {
    initScanner(scanner, sourceCode);
    return scanTokens(scanner);
}
