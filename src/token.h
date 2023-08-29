#ifndef delta_token_h
#define delta_token_h

// If modifying this enum, you should also update `tokenTypeStrings`.
typedef enum {
    // Keywords
    TOKEN_IF,
    TOKEN_ELSE,
    TOKEN_STRUCT,
    TOKEN_RETURN,
    TOKEN_FALSE,
    TOKEN_TRUE,
    TOKEN_NULL,
    TOKEN_VAL,

    // Identifier
    TOKEN_IDENTIFIER,

    // String literal
    TOKEN_STRING,

    // Number literal
    TOKEN_NUMBER,

    // Punctuation
    TOKEN_LEFT_PAREN,
    TOKEN_RIGHT_PAREN,
    TOKEN_LEFT_CURLY,
    TOKEN_RIGHT_CURLY,
    TOKEN_DOT,
    TOKEN_STAR,
    TOKEN_SLASH,
    TOKEN_PLUS,
    TOKEN_MINUS,
    TOKEN_EXCLAMATION,
    TOKEN_EXCLAMATION_EQUAL,
    TOKEN_MODULO,
    TOKEN_LESSER,
    TOKEN_LESSER_EQUAL,
    TOKEN_GREATER,
    TOKEN_GREATER_EQUAL,
    TOKEN_EQUAL,
    TOKEN_EQUAL_EQUAL,
    TOKEN_OR_OR,
    TOKEN_AND_AND,
    TOKEN_SEMICOLON,
    TOKEN_COMMA,

    // Special tokens
    TOKEN_ERROR,
    TOKEN_EOF
} TokenType;

// Get the string name for a token. We assume the order of the `TokenType` enum is the same as this array.
static char const* tokenTypeStrings[] = {
    // Keywords
    "TOKEN_IF",
    "TOKEN_ELSE",
    "TOKEN_STRUCT",
    "TOKEN_RETURN",
    "TOKEN_FALSE",
    "TOKEN_TRUE",
    "TOKEN_NULL",
    "TOKEN_VAL",

    // Identifier
    "TOKEN_IDENTIFIER",

    // String literal
    "TOKEN_STRING",

    // Number literal
    "TOKEN_NUMBER",

    // Punctuation
    "TOKEN_LEFT_PAREN",
    "TOKEN_RIGHT_PAREN",
    "TOKEN_LEFT_CURLY",
    "TOKEN_RIGHT_CURLY",
    "TOKEN_DOT",
    "TOKEN_STAR",
    "TOKEN_SLASH",
    "TOKEN_PLUS",
    "TOKEN_MINUS",
    "TOKEN_EXCLAMATION",
    "TOKEN_EXCLAMATION_EQUAL",
    "TOKEN_MODULO",
    "TOKEN_LESSER",
    "TOKEN_LESSER_EQUAL",
    "TOKEN_GREATER",
    "TOKEN_GREATER_EQUAL",
    "TOKEN_EQUAL",
    "TOKEN_EQUAL_EQUAL",
    "TOKEN_OR_OR",
    "TOKEN_AND_AND",
    "TOKEN_SEMICOLON",
    "TOKEN_COMMA",

    // Special tokens
    "TOKEN_ERROR",
    "TOKEN_EOF"};

typedef struct {
    TokenType type;
    const char* start;
    int length;
    int lineNo;
    int colNo;
} Token;

#endif