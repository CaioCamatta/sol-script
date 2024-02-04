#include "parser.h"

#include <stdbool.h>

#include "syntax.h"

// ---------------------------------------------------------------------------
// ------------------------- PARSER HELPER FUNCTIONS -------------------------
// ---------------------------------------------------------------------------

void initASTParser(ASTParser* parser, const TokenArray tokens) {
    parser->current = tokens.values;
    parser->tokenArray = tokens;
    parser->source = (Source*)malloc(sizeof(Source));
    parser->source->numberOfStatements = 0;
    parser->previous = NULL;
}

// Allocate an AST node (literal, expression, statement) on the heap and return a pointer to the allocated node.
#define allocateASTNode(type) (type*)malloc(sizeof(type));

// Free an AST node (literal, expression, statement) on the heap.
#define freeASTNode(node) free(node);

// Free memory allocated for the Source of an AST.
void freeSource(Source* source) {
    // TODO: add logic to make freeing recursive
    free(source);
}

// Print error at current token, halt execution
static void errorAtCurrent(ASTParser* parser, const char* message) {
    fprintf(stderr, "Error at line %d, column %d: %s", parser->current->lineNo, parser->current->colNo, message);
    exit(EXIT_FAILURE);
}

// Peek token to be immediately parsed.
static Token* peek(ASTParser* parser) {
    return parser->current;
}

// Move the current token to the next one.
static void advance(ASTParser* parser) {
    if (parser->current->type != TOKEN_EOF) {
        parser->previous = parser->current;
        parser->current++;
    }
}

// Consume provided type & advance, or error
static Token* consume(ASTParser* parser, TokenType type, const char* message) {
    if (parser->current->type == type) {
        Token* currToken = parser->current;
        advance(parser);
        return currToken;
    }

    errorAtCurrent(parser, message);
    return NULL;
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

// ---------------------------------------------------------------------------
// ------------------------------- PRODUCTIONS -------------------------------
// ---------------------------------------------------------------------------

/**
 * Terminal rule. Match identifier token.
 */
static Literal* identifierLiteral(ASTParser* parser) {
    Token* identifier = consume(parser, TOKEN_IDENTIFIER, "Expected identifier.");
    IdentifierLiteral* tempIdentifierLiteral = allocateASTNode(IdentifierLiteral);
    tempIdentifierLiteral->token = *(identifier);

    Literal* literal = allocateASTNode(Literal);
    literal->type = IDENTIFIER_LITERAL;
    literal->as.identifierLiteral = tempIdentifierLiteral;

    return literal;
}

/**
 * Terminal rule. Match number literal.
 *
 * Returns a pointer to a dynamically-allocated Literal.
 */
static Literal* numberLiteral(ASTParser* parser) {
    Token* currToken = consume(parser, TOKEN_NUMBER, "Expected number literal.");

    NumberLiteral* numberLiteral = allocateASTNode(NumberLiteral);
    numberLiteral->token = *(currToken);

    Literal* literal = allocateASTNode(Literal);
    literal->type = NUMBER_LITERAL;
    literal->as.numberLiteral = numberLiteral;

    return literal;
}

static Literal* booleanLiteral(ASTParser* parser) {
    Token* currToken = consume(parser, peek(parser)->type, "Expected boolean literal.");

    BooleanLiteral* booleanLiteral = allocateASTNode(BooleanLiteral);
    booleanLiteral->token = *currToken;

    Literal* literal = allocateASTNode(Literal);
    literal->type = BOOLEAN_LITERAL;
    literal->as.booleanLiteral = booleanLiteral;

    return literal;
}

/**
 * primary-expression:
 * number-literal
 * string-literal
 * identifier
 * ( expression )
 * "true"
 * "false"
 * "null"
 * "this"
 *
 * Returns a pointer to a dynamically-allocated PrimaryExpression.
 */
static Expression* primaryExpression(ASTParser* parser) {
    switch (peek(parser)->type) {
        case TOKEN_NUMBER: {
            Literal* tempLiteral = numberLiteral(parser);

            Expression* expression = allocateASTNode(Expression);
            expression->type = PRIMARY_EXPRESSION;

            PrimaryExpression* primaryExpression = allocateASTNode(PrimaryExpression);
            primaryExpression->literal = tempLiteral;

            expression->as.primaryExpression = primaryExpression;

            return expression;
            break;
        }
        case TOKEN_TRUE:
        case TOKEN_FALSE: {
            Literal* tempLiteral = booleanLiteral(parser);

            Expression* expression = allocateASTNode(Expression);
            expression->type = PRIMARY_EXPRESSION;

            PrimaryExpression* primaryExpression = allocateASTNode(PrimaryExpression);
            primaryExpression->literal = tempLiteral;

            expression->as.primaryExpression = primaryExpression;

            return expression;
            break;
        }
        default: {
            errorAtCurrent(parser, "Expected a primary expression.");
            return NULL;
            break;
        }
    }
}

/**
 * unary-expression:
 *  ( "!"* | "-"* )? postfix-expression
 */
static Expression* unaryExpression(ASTParser* parser) {
    if (match(parser, TOKEN_EXCLAMATION) || match(parser, TOKEN_MINUS)) {
        Token punctuator = *(parser->previous);                 // Capture the punctuator
        Expression* rightExpression = unaryExpression(parser);  // Recursively parse the right side

        UnaryExpression* unaryExpression = allocateASTNode(UnaryExpression);
        unaryExpression->punctuator = punctuator;
        unaryExpression->rightExpression = rightExpression;

        Expression* expression = allocateASTNode(Expression);
        expression->type = UNARY_EXPRESSION;
        expression->as.unaryExpression = unaryExpression;

        return expression;
    }
    return primaryExpression(parser);
}

/**
 * multiplicative-expression:
 *  unary-expression ( ( "/" | "*" ) unary-expression )*
 */
static Expression* multiplicativeExpression(ASTParser* parser) {
    Expression* leftExpression = unaryExpression(parser);
    while (match(parser, TOKEN_STAR) || match(parser, TOKEN_SLASH)) {
        Token punctuator = *(parser->previous);
        Expression* rightExpression = unaryExpression(parser);

        MultiplicativeExpression* multiplicativeExpression = allocateASTNode(MultiplicativeExpression);
        multiplicativeExpression->leftExpression = leftExpression;
        multiplicativeExpression->rightExpression = rightExpression;
        multiplicativeExpression->punctuator = punctuator;

        leftExpression = allocateASTNode(Expression);
        leftExpression->type = MULTIPLICATIVE_EXPRESSION;
        leftExpression->as.multiplicativeExpression = multiplicativeExpression;
    }
    return leftExpression;
}

/**
 * additive-expression:
 *  multiplicative-expression ( ( "-" | "+" ) multiplicative-expression )*
 */
static Expression* additiveExpression(ASTParser* parser) {
    Expression* leftExpression = multiplicativeExpression(parser);
    while (match(parser, TOKEN_PLUS) || match(parser, TOKEN_MINUS)) {
        Token punctuator = *(parser->previous);
        Expression* rightExpression = multiplicativeExpression(parser);

        AdditiveExpression* additiveExpression = allocateASTNode(AdditiveExpression);
        additiveExpression->leftExpression = leftExpression;
        additiveExpression->rightExpression = rightExpression;
        additiveExpression->punctuator = punctuator;

        leftExpression = allocateASTNode(Expression);
        leftExpression->type = ADDITIVE_EXPRESSION;
        leftExpression->as.additiveExpression = additiveExpression;
    }
    return leftExpression;
}

/**
 * comparison-expression:
 *  additive-expression ( ( ">" | ">=" | "<" | "<=" ) additive-expression )*
 */
static Expression* comparisonExpression(ASTParser* parser) {
    Expression* leftExpression = additiveExpression(parser);
    while (match(parser, TOKEN_GREATER) || match(parser, TOKEN_GREATER_EQUAL) ||
           match(parser, TOKEN_LESSER) || match(parser, TOKEN_LESSER_EQUAL)) {
        Token punctuator = *(parser->previous);
        Expression* rightExpression = additiveExpression(parser);

        ComparisonExpression* comparisonExpression = allocateASTNode(ComparisonExpression);
        comparisonExpression->leftExpression = leftExpression;
        comparisonExpression->rightExpression = rightExpression;
        comparisonExpression->punctuator = punctuator;

        leftExpression = allocateASTNode(Expression);
        leftExpression->type = COMPARISON_EXPRESSION;
        leftExpression->as.comparisonExpression = comparisonExpression;
    }
    return leftExpression;
}

/**
 * equality-expression:
 *  comparison-expression ( ("!=" | "==") comparison-expression) )*
 */
static Expression* equalityExpression(ASTParser* parser) {
    Expression* leftExpression = comparisonExpression(parser);
    while (match(parser, TOKEN_EQUAL_EQUAL) || match(parser, TOKEN_EXCLAMATION_EQUAL)) {
        Token punctuator = *(parser->previous);
        Expression* rightExpression = comparisonExpression(parser);

        EqualityExpression* equalityExpression = allocateASTNode(EqualityExpression);
        equalityExpression->leftExpression = leftExpression;
        equalityExpression->rightExpression = rightExpression;
        equalityExpression->punctuator = punctuator;

        leftExpression = allocateASTNode(Expression);
        leftExpression->type = EQUALITY_EXPRESSION;
        leftExpression->as.equalityExpression = equalityExpression;
    }
    return leftExpression;
}

/**
 * logical-and-expression:
 *  equality-expression ( "and" equality-expression )*
 */
static Expression* logicalAndExpression(ASTParser* parser) {
    Expression* leftExpression = equalityExpression(parser);
    while (match(parser, TOKEN_AND_AND)) {
        Expression* rightExpression = equalityExpression(parser);

        LogicalAndExpression* logicalAndExpression = allocateASTNode(LogicalAndExpression);
        logicalAndExpression->leftExpression = leftExpression;
        logicalAndExpression->rightExpression = rightExpression;

        leftExpression = allocateASTNode(Expression);
        leftExpression->type = LOGICAL_AND_EXPRESSION;
        leftExpression->as.logicalAndExpression = logicalAndExpression;
    }
    return leftExpression;
}

/**
 * logical-or-expression:
 *  logical-and-expression ( "or" logical-and-expression )*
 */
static Expression* logicalOrExpression(ASTParser* parser) {
    Expression* leftExpression = logicalAndExpression(parser);
    while (match(parser, TOKEN_OR_OR)) {
        Expression* rightExpression = logicalAndExpression(parser);

        LogicalOrExpression* logicalOrExpression = allocateASTNode(LogicalOrExpression);
        logicalOrExpression->leftExpression = leftExpression;
        logicalOrExpression->rightExpression = rightExpression;

        Expression* expression = allocateASTNode(Expression);
        expression->type = LOGICAL_OR_EXPRESSION;
        expression->as.logicalOrExpression = logicalOrExpression;
        return expression;
    }
    return leftExpression;
}

/**
 * expression:
 *  struct-expression
 *  function-expression
 *  block-expression
 *  logical-or-expression
 */
static Expression* expression(ASTParser* parser) {
    return logicalOrExpression(parser);
}

/**
 * val-declaration:
 *  "val" identifier ( "=" expression )? ";"
 */
static Statement* valDeclaration(ASTParser* parser) {
    consume(parser, TOKEN_VAL, "Expected 'val' in a val declaration.");
    Literal* tempIdentifier = identifierLiteral(parser);
    consume(parser, TOKEN_EQUAL, "Expected '=' in a val declaration.");
    Expression* tempExpression = expression(parser);
    consume(parser, TOKEN_SEMICOLON, "Expected semicolon");

    ValDeclarationStatement* valDeclarationStatement = allocateASTNode(ValDeclarationStatement);
    valDeclarationStatement->identifier = tempIdentifier->as.identifierLiteral;
    valDeclarationStatement->expression = tempExpression;

    // Wrap ValDeclarationStatement in a Statement
    Statement* statement = allocateASTNode(Statement);
    statement->type = VAL_DECLARATION_STATEMENT;
    statement->as.valDeclarationStatement = valDeclarationStatement;
    return statement;
}

/**
 * declaration:
 *  var-declaration ";"
 *  val-declaration ";"
 */
static Statement* declaration(ASTParser* parser) {
    if (peek(parser)->type == TOKEN_VAL) {
        return valDeclaration(parser);
        // TODO: add VAR handling
    } else {
        errorAtCurrent(parser, "Expected 'var' or 'val' in a declaration.");  // Unreachable
        return NULL;
    }
}

/**
 * print-statement:
 *  "print" expression ";"
 */
static Statement* printStatement(ASTParser* parser) {
    consume(parser, TOKEN_PRINT, "Expected 'print' in a print statement.");
    Expression* expr = expression(parser);
    consume(parser, TOKEN_SEMICOLON, "Expected ';' after print statement.");

    PrintStatement* printStatement = allocateASTNode(PrintStatement);
    printStatement->expression = expr;

    Statement* stmt = allocateASTNode(Statement);
    stmt->type = PRINT_STATEMENT;
    stmt->as.printStatement = printStatement;

    return stmt;
}

static Statement* expressionStatement(ASTParser* parser) {
    Expression* expr = expression(parser);
    consume(parser, TOKEN_SEMICOLON, "Expected ';' after expression.");

    ExpressionStatement* exprStmt = allocateASTNode(ExpressionStatement);
    exprStmt->expression = expr;

    Statement* stmt = allocateASTNode(Statement);
    stmt->type = EXPRESSION_STATEMENT;
    stmt->as.expressionStatement = exprStmt;

    return stmt;
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
 *  print-statement
 */
static Statement* statement(ASTParser* parser) {
    // NOTE
    // I don't love this, but in this `statement` function, but to make the parser efficient,
    // we need logic from the child productions. We need look ahead to see if the next token
    // matches the first token of a child production. If it does, we need to call that child.
    // This could be refactored so the logic to "check" is in the child functions; it'd be
    // cleaner IMO but probably less efficient.
    TokenType currentType = peek(parser)->type;
    switch (currentType) {
        // case TOKEN_VAR:
        case TOKEN_VAL:
            return declaration(parser);
        case TOKEN_PRINT: {
            return printStatement(parser);
        }
        // case TOKEN_LEFT_CURLY:
        //     return blockStatement(parser);
        // case TOKEN_WHILE:
        //     return iterationStatement(parser);
        // case TOKEN_IF:
        //     return selectionStatement(parser);
        // case TOKEN_RETURN:
        //     return returnStatement(parser);
        default:
            return expressionStatement(parser);
    }
}

/**
 * Entry point for a program.
 *
 * source:
 *  statement* EOF
 */
static void source(ASTParser* parser) {
    while (!check(parser, TOKEN_EOF)) {
        if (parser->source->numberOfStatements >= MAX_NUMBER_STATEMENTS) {
            errorAtCurrent(parser, "Exceeded maximum number of statements.");
        }

        Statement* statementNode = statement(parser);

        if (statementNode != NULL) {
            parser->source->rootStatements[parser->source->numberOfStatements++] = statementNode;
        } else {
            exit(EXIT_FAILURE);
        }
    }
}

Source* parseAST(ASTParser* parser) {
    source(parser);
    return parser->source;
}

Source* parseASTFromTokens(ASTParser* parser, TokenArray* tokenArray) {
    initASTParser(parser, *tokenArray);
    Source* source = parseAST(parser);
    return source;
}