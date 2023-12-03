#ifndef delta_syntax_h
#define delta_syntax_h

#include "token.h"
#include "util/array.h"

// Forward declarations
typedef struct ExpressionStatement ExpressionStatement;
typedef struct AdditiveExpression AdditiveExpression;
typedef struct PrimaryExpression PrimaryExpression;
typedef struct NumberLiteral NumberLiteral;

// --- Types ---
typedef enum {
    EXPRESSION_STATEMENT
} StatementType;

typedef enum {
    ADDITIVE_EXPRESSION,
    PRIMARY_EXPRESSION
} ExpressionType;

typedef enum {
    NUMBER_LITERAL
} LiteralType;

// --- Abstract productions ---
typedef struct {
    StatementType type;
    union {
        ExpressionStatement *expressionStatement;
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
    } as;
} Literal;

// --- Concrete productions ---
struct ExpressionStatement {
    Expression *expression;
};

struct AdditiveExpression {
    Expression *leftExpression;
    Expression *rightExpression;
    Token *punctuator;
};

struct PrimaryExpression {
    Literal *literal;
};

struct NumberLiteral {
    Token token;
};

/**
 * The 'source' node of the AST.
 */
static const int MAX_NUMBER_STATEMENTS = 10000;  // TODO: use dynamic array
typedef struct {
    Statement statements[MAX_NUMBER_STATEMENTS];
    int numberOfStatements;
} Source;

#endif