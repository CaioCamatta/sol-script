#ifndef sol_script_syntax_h
#define sol_script_syntax_h

#include "token.h"
#include "util/array.h"

// Forward declarations
typedef struct ExpressionStatement ExpressionStatement;
typedef struct PrintStatement PrintStatement;
typedef struct ValDeclarationStatement ValDeclarationStatement;
typedef struct LogicalOrExpression LogicalOrExpression;
typedef struct LogicalAndExpression LogicalAndExpression;
typedef struct EqualityExpression EqualityExpression;
typedef struct ComparisonExpression ComparisonExpression;
typedef struct AdditiveExpression AdditiveExpression;
typedef struct MultiplicativeExpression MultiplicativeExpression;
typedef struct UnaryExpression UnaryExpression;
typedef struct PrimaryExpression PrimaryExpression;
typedef struct BooleanLiteral BooleanLiteral;
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
    LOGICAL_OR_EXPRESSION,
    LOGICAL_AND_EXPRESSION,
    EQUALITY_EXPRESSION,
    COMPARISON_EXPRESSION,
    ADDITIVE_EXPRESSION,
    MULTIPLICATIVE_EXPRESSION,
    UNARY_EXPRESSION,
    PRIMARY_EXPRESSION
} ExpressionType;

typedef enum {
    BOOLEAN_LITERAL,
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
        LogicalOrExpression *logicalOrExpression;
        LogicalAndExpression *logicalAndExpression;
        EqualityExpression *equalityExpression;
        ComparisonExpression *comparisonExpression;
        AdditiveExpression *additiveExpression;
        MultiplicativeExpression *multiplicativeExpression;
        UnaryExpression *unaryExpression;
        PrimaryExpression *primaryExpression;
    } as;
} Expression;

typedef struct {
    LiteralType type;
    union {
        BooleanLiteral *booleanLiteral;
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

struct ValDeclarationStatement {
    IdentifierLiteral *identifier;
    Expression *expression;
};

struct LogicalOrExpression {
    Expression *leftExpression;
    Expression *rightExpression;
};

struct LogicalAndExpression {
    Expression *leftExpression;
    Expression *rightExpression;
};

struct EqualityExpression {
    Expression *leftExpression;
    Expression *rightExpression;
    Token punctuator;
};

struct ComparisonExpression {
    Expression *leftExpression;
    Expression *rightExpression;
    Token punctuator;
};

struct AdditiveExpression {
    Expression *leftExpression;
    Expression *rightExpression;
    Token punctuator;
};

struct MultiplicativeExpression {
    Expression *leftExpression;
    Expression *rightExpression;
    Token punctuator;
};

struct UnaryExpression {
    Token punctuator;
    Expression *rightExpression;
};

struct PrimaryExpression {
    Literal *literal;
};

struct BooleanLiteral {
    Token token;
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