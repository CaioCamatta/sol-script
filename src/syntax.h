#ifndef sol_script_syntax_h
#define sol_script_syntax_h

#include "token.h"
#include "util/array.h"

// Forward declarations
typedef struct ExpressionStatement ExpressionStatement;
typedef struct PrintStatement PrintStatement;
typedef struct BlockStatement BlockStatement;
typedef struct ValDeclarationStatement ValDeclarationStatement;
typedef struct VarDeclarationStatement VarDeclarationStatement;
typedef struct SelectionStatement SelectionStatement;
typedef struct IterationStatement IterationStatement;
typedef struct AssignmentStatement AssignmentStatement;
typedef struct LogicalOrExpression LogicalOrExpression;
typedef struct LogicalAndExpression LogicalAndExpression;
typedef struct EqualityExpression EqualityExpression;
typedef struct ComparisonExpression ComparisonExpression;
typedef struct AdditiveExpression AdditiveExpression;
typedef struct MultiplicativeExpression MultiplicativeExpression;
typedef struct BlockExpression BlockExpression;
typedef struct UnaryExpression UnaryExpression;
typedef struct PrimaryExpression PrimaryExpression;
typedef struct BooleanLiteral BooleanLiteral;
typedef struct NumberLiteral NumberLiteral;
typedef struct IdentifierLiteral IdentifierLiteral;
typedef struct StringLiteral StringLiteral;

// --- Types ---
// (Stack effect is how the instruction modifies the stack, +1 means it adds something to the stack.)
typedef enum {
    EXPRESSION_STATEMENT,       // Stack effect: 0
    VAL_DECLARATION_STATEMENT,  // Stack effect: +1 if local, 0 if global.
    VAR_DECLARATION_STATEMENT,  // Stack effect: +1 if local, 0 if global.
    PRINT_STATEMENT,            // Stack effect: 0
    BLOCK_STATEMENT,            // Stack effect: 0
    SELECTION_STATEMENT,        // Stack effect: 1
    ASSIGNMENT_STATEMENT,       // Stack effect: 0
    ITERATION_STATEMENT         // Stack effect: 0
} StatementType;

typedef enum {
    LOGICAL_OR_EXPRESSION,      // Stack effect: -1
    LOGICAL_AND_EXPRESSION,     // Stack effect: -1
    EQUALITY_EXPRESSION,        // Stack effect: -1
    COMPARISON_EXPRESSION,      // Stack effect: -1
    ADDITIVE_EXPRESSION,        // Stack effect: -1
    MULTIPLICATIVE_EXPRESSION,  // Stack effect: -1
    BLOCK_EXPRESSION,           // Stack effect: 0 per se
    UNARY_EXPRESSION,           // Stack effect: 0
    PRIMARY_EXPRESSION          // Stack effect: 0 per se
} ExpressionType;

typedef enum {
    BOOLEAN_LITERAL,     // Stack effect: +1
    NUMBER_LITERAL,      // Stack effect: +1
    IDENTIFIER_LITERAL,  // Stack effect: +1
    STRING_LITERAL       // Stack effect: +1
} LiteralType;

// --- Abstract productions ---
typedef struct {
    StatementType type;
    union {
        ExpressionStatement *expressionStatement;
        ValDeclarationStatement *valDeclarationStatement;
        VarDeclarationStatement *varDeclarationStatement;
        PrintStatement *printStatement;
        BlockStatement *blockStatement;
        SelectionStatement *selectionStatement;
        AssignmentStatement *assignmentStatement;
        IterationStatement *iterationStatement;
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
        BlockExpression *blockExpression;
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

typedef struct {
    Statement **values;
    size_t used;
    size_t size;
} StatementArray;

struct BlockStatement {
    StatementArray statementArray;
};

struct PrintStatement {
    Expression *expression;
};

struct ValDeclarationStatement {
    IdentifierLiteral *identifier;
    Expression *expression;
};

struct VarDeclarationStatement {
    IdentifierLiteral *identifier;
    Expression *maybeExpression;
};

struct SelectionStatement {
    Expression *conditionExpression;
    Statement *trueStatement;   // The parser will enforce that these statements are BlockStatements
    Statement *falseStatement;  // NULL if there's no else
};

struct AssignmentStatement {
    Expression *target;
    Expression *value;
};

struct IterationStatement {
    Expression *conditionExpression;
    Statement *bodyStatement;
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

struct BlockExpression {
    StatementArray statementArray;
    Expression *lastExpression;
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