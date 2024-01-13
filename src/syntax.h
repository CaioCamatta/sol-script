#ifndef delta_syntax_h
#define delta_syntax_h

#include "token.h"
#include "util/array.h"

// Forward declarations
typedef struct ExpressionStatement ExpressionStatement;
typedef struct PrintStatement PrintStatement;
typedef struct ValDeclarationStatement ValDeclarationStatement;
typedef struct AdditiveExpression AdditiveExpression;
typedef struct PrimaryExpression PrimaryExpression;
typedef struct NumberLiteral NumberLiteral;
typedef struct IdentifierLiteral IdentifierLiteral;
typedef struct StringLiteral StringLiteral;

// --- Types ---
typedef enum {
    EXPRESSION_STATEMENT,
    VAL_DECLARATION_STATEMENT,
    PRINT_STATEMENT
} StatementType;

typedef enum {
    ADDITIVE_EXPRESSION,
    PRIMARY_EXPRESSION
} ExpressionType;

typedef enum {
    NUMBER_LITERAL,
    IDENTIFIER_LITERAL,
    STRING_LITERAL
} LiteralType;

// --- Abstract productions ---
typedef struct {
    StatementType type;
    union {
        ExpressionStatement *expressionStatement;
        ValDeclarationStatement *valDeclarationStatement;
        PrintStatement *printStatement;
    } as;
} Statement;

typedef struct {
    ExpressionType type;
    union {
        PrimaryExpression *primaryExpression;
        AdditiveExpression *additiveExpression;
    } as;
} Expression;

typedef struct {
    LiteralType type;
    union {
        NumberLiteral *numberLiteral;
        IdentifierLiteral *identifierLiteral;
        StringLiteral *stringLiteral;
    } as;
} Literal;

// --- Concrete productions ---
struct ExpressionStatement {
    Expression *expression;
};

struct PrintStatement {
    Expression *expression;
};

struct AdditiveExpression {
    Expression *leftExpression;
    Expression *rightExpression;
    Token *punctuator;
};

struct ValDeclarationStatement {
    IdentifierLiteral *identifier;
    Expression *expression;
};

struct PrimaryExpression {
    Literal *literal;
};

struct NumberLiteral {
    Token token;
};

struct IdentifierLiteral {
    Token token;
};

struct StringLiteral {
    Token token;
};

/**
 * The 'source' node of the AST. It's named the same as Scala's 'Source'.
 */
static const int MAX_NUMBER_STATEMENTS = 10000;  // TODO: use dynamic array
typedef struct {
    Statement *rootStatements[MAX_NUMBER_STATEMENTS];
    int numberOfStatements;
} Source;

#endif