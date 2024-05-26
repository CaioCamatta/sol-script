#ifndef sol_script_token_h
#define sol_script_token_h

// If modifying this enum, you should also update `tokenTypeToString`.
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
    TOKEN_VAR,
    TOKEN_PRINT,
    TOKEN_WHILE,
    TOKEN_LAMBDA,

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

typedef struct {
    TokenType type;
    const char* start;  // Identifier tokens are not null-terminated!
    int length;
    int lineNo;
    int colNo;
} Token;

#endif