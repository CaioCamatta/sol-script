#include "scanner.h"

#include "../minunit.h"
#include "debug.h"
#include "file.h"

int test_scanner() {
    const TokenType expectedTokens[] = {TOKEN_IF, TOKEN_ELSE, TOKEN_STRUCT, TOKEN_RETURN, TOKEN_FALSE, TOKEN_TRUE, TOKEN_NULL, TOKEN_VAL, TOKEN_IDENTIFIER, TOKEN_STRING, TOKEN_NUMBER, TOKEN_NUMBER, TOKEN_NUMBER, TOKEN_LEFT_PAREN, TOKEN_RIGHT_PAREN, TOKEN_LEFT_CURLY, TOKEN_RIGHT_CURLY, TOKEN_DOT, TOKEN_STAR, TOKEN_SLASH, TOKEN_PLUS, TOKEN_MINUS, TOKEN_EXCLAMATION, TOKEN_EXCLAMATION_EQUAL, TOKEN_MODULO, TOKEN_LESSER, TOKEN_LESSER_EQUAL, TOKEN_GREATER, TOKEN_GREATER_EQUAL, TOKEN_OR_OR, TOKEN_AND_AND, TOKEN_SEMICOLON, TOKEN_COMMA, TOKEN_ERROR};

    char* sourceCode = readFile("./test/unit/src/all_tokens.d");
    Scanner scanner;

    initScanner(&scanner, sourceCode);
    TokenArray tokens = scan(&scanner);

    for (size_t i = 0; i < tokens.used; i++) {
        TokenType actualToken = tokens.values[i].type;
        TokenType expectedToken = expectedTokens[i];
        ASSERT_WITH_MESSAGE(actualToken == expectedToken,
                            printf("ERROR - %s is not equal to %s.\n", tokenTypeStrings[actualToken], tokenTypeStrings[(int)expectedToken]));
    }

    return SUCCESS_RETURN_CODE;
}