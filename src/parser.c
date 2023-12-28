#include "parser.h"

#include <stdbool.h>

#include "syntax.h"

// ---------------------------------------------------------------------------
// ------------------------- PARSER HELPER FUNCTIONS -------------------------
// ---------------------------------------------------------------------------

void initTreeParser(ASTParser* parser, const TokenArray tokens) {
    parser->current = tokens.values;
    parser->tokenArray = tokens;
    parser->source = (Source*)malloc(sizeof(Source));
    parser->source->numberOfStatements = 0;
    parser->currSyntaxType = STATEMENT;
}
// Allocate memory for a new Statement on the heap and return a pointer to the allocated Statement.
Statement* allocateStatement() {
    Statement* newStatement = (Statement*)malloc(sizeof(Statement));
    return newStatement;
}

//  Allocate memory for a new Expression on the heap and return a pointer to the allocated Expression.
Expression* allocateExpression() {
    Expression* newExpression = (Expression*)malloc(sizeof(Expression));
    return newExpression;
}

//  Allocate memory for a new Literal on the heap and return a pointer to the allocated Literal.
Literal* allocateLiteral() {
    Literal* newLiteral = (Literal*)malloc(sizeof(Literal));
    return newLiteral;
}

// Allocate an AST node (literal, expression, statement) on the heap and return a pointer to the allocated node.
#define allocateASTNode(type) (type*)malloc(sizeof(type));

// Free memory allocated for a Statement.
void freeStatement(Statement* statement) {
    free(statement);
}

// Free memory allocated for an Expression.
void freeExpression(Expression* expression) {
    free(expression);
}

// Free memory allocated for a Literal.
void freeLiteral(Literal* literal) {
    free(literal);
}

// Free memory allocated for an AST (the Source).
void freeParseTree(ASTParser* parser) {
    // TODO: add logic to make freeing recursive
    free(parser->source);
}

// Move the current token to the next one.
static void advance(ASTParser* parser) {
    if (parser->current->type != TOKEN_EOF)
        parser->current++;
}

// Consume provided type & advance, or error
static void consume(ASTParser* parser, TokenType type, const char* message) {
    if (parser->current->type == type) {
        advance(parser);
        return;
    }

    errorAtCurrent(message);
}

/* Check if current token is of a given type. */
static bool check(ASTParser* parser, TokenType type) {
    return parser->current->type == type;
}

/* Consume current token if it's of a given type. Returns true if a token was consumed and false otherwise. */
static bool match(ASTParser* parser, TokenType type) {
    if (check(parser, type)) {
        advance(parser);
        return true;
    }
    return false;
}

/**
 * Check if an expression corresponding to a grammar production is used / is true. If so, allow
 * side effects to happen (i.e., allow the tokens used to be consumed). If not, reset the
 * `ASTParser` `current` pointer to where it was before the `expr` was evaluated.
 *
 * This is macro is necessary the `expr` evaluation has the side effect of moving the
 * `current` pointer.
 *
 * IMPORTANT: this macro assumes 'parser' is in scope.
 *
 * Args:
 *  expr: a C expression, e.g. `match(parser, TOKEN_VAL) && identifier(parser)`
 */
#define commitIfProductionIsUsed(expr)                          \
    do {                                                        \
        Token* currentTokenBeforeTransaction = parser->current; \
        if (!(expr)) {                                          \
            parser->current = currentTokenBeforeTransaction;    \
            return false;                                       \
        }                                                       \
        return true;                                            \
    } while (0)

// ---------------------------------------------------------------------------
// ------------------------------- PRODUCTIONS -------------------------------
// ---------------------------------------------------------------------------

/**
 * Terminal rule. Match identifier token.
 *
 * Returns true if this production was used, false otherwise.
 */
static bool identifier(ASTParser* parser) {
    match(parser->current, TOKEN_IDENTIFIER);
}

/**
 * Terminal rule. Match number literal.
 *
 * Returns true if this production was used, false otherwise.
 */
static bool numberLiteral(ASTParser* parser) {
    match(parser->current, TOKEN_NUMBER);
}

/**
 * additive-expression:
 *  multiplicative-expression ( ( "-" | "+" ) multiplicative-expression )* ;
 *
 * Returns true if this production was used, false otherwise.
 */
static AdditiveExpression additiveExpression(ASTParser* parser) {
    commitIfProductionIsUsed(numberLiteral(parser) && match(parser, TOKEN_PLUS) && numberLiteral(parser));

    AdditiveExpression* additiveExpression = allocateASTNode(AdditiveExpression);
    additiveExpression->leftExpression = allocateExpression();
}

/**
 * expression:
 *  struct-expression
 *  function-expression
 *  block-expression
 *  assignment-expression
 *
 * Returns true if this production was used, false otherwise.
 */
static bool expression(ASTParser* parser) {
    additiveExpression(parser);
}

/**
 * val-declaration:
 *  "val" identifier ( "=" expression )?
 */
static bool valDeclaration(ASTParser* parser) {
    commitIfProductionIsUsed(match(parser, TOKEN_VAL) && identifier(parser) && match(parser, TOKEN_EQUAL) && expression(parser));
}

/**
 * declaration:
 *  var-declaration ";"
 *  val-declaration ";"
 *
 * Returns true if this production was used, false otherwise.
 */
static bool declaration(ASTParser* parser) {
    commitIfProductionIsUsed(valDeclaration && match(parser, TOKEN_SEMICOLON));
}

/**
 * expression-statement:
 *  expression ";"
 *
 * Returns true if this production was used, false otherwise.
 */
static bool expressionStatement(ASTParser* parser) {
    commitIfProductionIsUsed(expression(parser) && match(parser, TOKEN_SEMICOLON));
}

/**
 * statement:
 *  declaration
 *  block-statement
 *  iteration-statement
 *  selection-statement
 *  return-statement
 *  expression-statement
 *  assignment-statement
 */
static void statement(ASTParser* parser) {
    // Short-circuit
    declaration(parser) || expressionStatement(parser);
}

/**
 * Entry point for a program.
 *
 * source:
 *  statement* EOF
 */
static void source(ASTParser* parser) {
    while (parser->current != TOKEN_EOF) {
        statement(parser);
    }
}

TokenArray parseAST(ASTParser* parser) {
    source(parser);
}
