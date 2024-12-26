#include "parser.h"

#include <stdbool.h>
#include <string.h>

#include "config.h"
#include "debug.h"
#include "error.h"
#include "syntax.h"
#include "util/colors.h"

#define MAX_NUM_PARAMETERS_IN_FUNCTION UINT8_MAX

// ---------------------------------------------------------------------------
// ------------------------- PARSER HELPER FUNCTIONS -------------------------
// ---------------------------------------------------------------------------

static void reportParserErrorAndSynchronize(ASTParser* parser, const char* message);

void initASTParser(ASTParser* parser, const TokenArray tokens) {
    parser->current = tokens.values;
    parser->tokenArray = tokens;
    parser->source = (Source*)malloc(sizeof(Source));
    parser->source->numberOfStatements = 0;
    parser->previous = NULL;
    initErrorArray(&parser->errors);
}

// Allocate an AST node (literal, expression, statement) on the heap and return a pointer to the allocated node.
#define allocateASTNode(type) (type*)malloc(sizeof(type));

void freeStatement(Statement* stmt);  // Forward declaration

void freeLiteral(Literal* literal) {
    if (!literal) return;
    switch (literal->type) {
        case BOOLEAN_LITERAL:
            free(literal->as.booleanLiteral);
            break;
        case NUMBER_LITERAL:
            free(literal->as.numberLiteral);
            break;
        case IDENTIFIER_LITERAL:
            free(literal->as.identifierLiteral);
            break;
        case STRING_LITERAL:
            free(literal->as.stringLiteral);
            break;
    }
    free(literal);
}

void freeExpression(Expression* expr) {
    if (!expr) return;

    switch (expr->type) {
        case LOGICAL_OR_EXPRESSION:
            freeExpression(expr->as.logicalOrExpression->leftExpression);
            freeExpression(expr->as.logicalOrExpression->rightExpression);
            free(expr->as.logicalOrExpression);
            break;
        case LOGICAL_AND_EXPRESSION:
            freeExpression(expr->as.logicalAndExpression->leftExpression);
            freeExpression(expr->as.logicalAndExpression->rightExpression);
            free(expr->as.logicalAndExpression);
            break;
        case EQUALITY_EXPRESSION:
            freeExpression(expr->as.equalityExpression->leftExpression);
            freeExpression(expr->as.equalityExpression->rightExpression);
            free(expr->as.equalityExpression);
            break;
        case COMPARISON_EXPRESSION:
            freeExpression(expr->as.comparisonExpression->leftExpression);
            freeExpression(expr->as.comparisonExpression->rightExpression);
            free(expr->as.comparisonExpression);
            break;
        case ADDITIVE_EXPRESSION:
            freeExpression(expr->as.additiveExpression->leftExpression);
            freeExpression(expr->as.additiveExpression->rightExpression);
            free(expr->as.additiveExpression);
            break;
        case MULTIPLICATIVE_EXPRESSION:
            freeExpression(expr->as.multiplicativeExpression->leftExpression);
            freeExpression(expr->as.multiplicativeExpression->rightExpression);
            free(expr->as.multiplicativeExpression);
            break;
        case BLOCK_EXPRESSION:
            for (size_t i = 0; i < expr->as.blockExpression->statementArray.used; i++) {
                freeStatement(expr->as.blockExpression->statementArray.values[i]);
            }
            FREE_ARRAY(expr->as.blockExpression->statementArray);
            freeExpression(expr->as.blockExpression->lastExpression);
            free(expr->as.blockExpression);
            break;
        case UNARY_EXPRESSION:
            freeExpression(expr->as.unaryExpression->rightExpression);
            free(expr->as.unaryExpression);
            break;
        case PRIMARY_EXPRESSION:
            freeLiteral(expr->as.primaryExpression->literal);
            free(expr->as.primaryExpression);
            break;
        case LAMBDA_EXPRESSION:
            FREE_ARRAY((*(expr->as.lambdaExpression->parameters)));
            free(expr->as.lambdaExpression->parameters);
            freeExpression((Expression*)expr->as.lambdaExpression->bodyBlock);
            free(expr->as.lambdaExpression);
            break;
        case CALL_EXPRESSION:
            freeExpression(expr->as.callExpression->leftHandSide);
            for (size_t i = 0; i < expr->as.callExpression->arguments->used; i++) {
                freeExpression(expr->as.callExpression->arguments->values[i]);
            }
            FREE_ARRAY((*(expr->as.callExpression->arguments)));
            free(expr->as.callExpression->arguments);
            free(expr->as.callExpression);
            break;
        case MEMBER_EXPRESSION:
            freeExpression(expr->as.memberExpression->leftHandSide);
            freeExpression(expr->as.memberExpression->rightHandSide);
            free(expr->as.memberExpression);
            break;
        case STRUCT_EXPRESSION:
            for (size_t i = 0; i < expr->as.structExpression->declarationArray.used; i++) {
                StructDeclaration* decl = expr->as.structExpression->declarationArray.values[i];
                freeLiteral((Literal*)decl->identifier);
                freeExpression(decl->maybeExpression);
                free(decl);
            }
            FREE_ARRAY(expr->as.structExpression->declarationArray);
            free(expr->as.structExpression);
            break;
    }
    free(expr);
}

void freeStatement(Statement* stmt) {
    if (!stmt) return;

    switch (stmt->type) {
        case EXPRESSION_STATEMENT:
            freeExpression(stmt->as.expressionStatement->expression);
            free(stmt->as.expressionStatement);
            break;
        case VAL_DECLARATION_STATEMENT:
            freeLiteral((Literal*)stmt->as.valDeclarationStatement->identifier);
            freeExpression(stmt->as.valDeclarationStatement->expression);
            free(stmt->as.valDeclarationStatement);
            break;
        case VAR_DECLARATION_STATEMENT:
            freeLiteral((Literal*)stmt->as.varDeclarationStatement->identifier);
            freeExpression(stmt->as.varDeclarationStatement->maybeExpression);
            free(stmt->as.varDeclarationStatement);
            break;
        case PRINT_STATEMENT:
            freeExpression(stmt->as.printStatement->expression);
            free(stmt->as.printStatement);
            break;
        case BLOCK_STATEMENT:
            for (size_t i = 0; i < stmt->as.blockStatement->statementArray.used; i++) {
                freeStatement(stmt->as.blockStatement->statementArray.values[i]);
            }
            FREE_ARRAY(stmt->as.blockStatement->statementArray);
            free(stmt->as.blockStatement);
            break;
        case SELECTION_STATEMENT:
            freeExpression(stmt->as.selectionStatement->conditionExpression);
            freeStatement(stmt->as.selectionStatement->trueStatement);
            if (stmt->as.selectionStatement->falseStatement) {
                freeStatement(stmt->as.selectionStatement->falseStatement);
            }
            free(stmt->as.selectionStatement);
            break;
        case ASSIGNMENT_STATEMENT:
            freeExpression(stmt->as.assignmentStatement->target);
            freeExpression(stmt->as.assignmentStatement->value);
            free(stmt->as.assignmentStatement);
            break;
        case ITERATION_STATEMENT:
            freeExpression(stmt->as.iterationStatement->conditionExpression);
            freeStatement(stmt->as.iterationStatement->bodyStatement);
            free(stmt->as.iterationStatement);
            break;
        case RETURN_STATEMENT:
            freeExpression(stmt->as.returnStatement->expression);
            free(stmt->as.returnStatement);
            break;
    }
    free(stmt);
}

// Free the memory allocated for the Source of an AST.
void freeSource(Source* source) {
    if (!source) return;
    for (int i = 0; i < source->numberOfStatements; i++) {
        freeStatement(source->rootStatements[i]);
    }
    free(source);
}

// Free parser and AST, but not the Token array or the original code string.
void freeParserButNotAST(ASTParser* parser) {
    FREE_ARRAY(parser->errors);

    if (parser->source) {
        freeSource(parser->source);
        parser->source = NULL;
    }

    parser->current = NULL;
    parser->previous = NULL;
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
    Token* currToken = parser->current;
    if (parser->current->type == type) {
        advance(parser);
        return currToken;
    }

    reportParserErrorAndSynchronize(parser, message);
    return currToken;
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

/* Print an error token produced by the scanner. */
#define printErrorToken(parser, messagePrefix, errorToken)                      \
    do {                                                                        \
        char errorMessage[parser->current->length + strlen(messagePrefix)];     \
        strcpy(errorMessage, messagePrefix);                                    \
        strncat(errorMessage, parser->current->start, parser->current->length); \
        reportParserErrorAndSynchronize(parser, errorMessage);                  \
    } while (0);

/**
 * Check that there is a semicolon after an expression.
 * */
static void checkSemicolonAfterExpression(ASTParser* parser, Expression* maybeLatestExpression, char* message) {
    if (maybeLatestExpression && maybeLatestExpression->type == BLOCK_EXPRESSION) {
        match(parser, TOKEN_SEMICOLON);
    } else {
        consume(parser, TOKEN_SEMICOLON, message);
    }
}

/**
 * Recover from an error. Skip tokens until we finds a synchronization point, which
 * we define as being the start of the next statement.
 */
static void synchronizeAfterError(ASTParser* parser) {
    advance(parser);

    while (!check(parser, TOKEN_EOF)) {
        if (parser->previous->type == TOKEN_SEMICOLON) return;

        switch (peek(parser)->type) {
            case TOKEN_VAL:
            case TOKEN_VAR:
            case TOKEN_PRINT:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_RETURN:
            case TOKEN_LAMBDA:
                return;
            default:
                advance(parser);
        }
    }
}

static void reportParserErrorAndSynchronize(ASTParser* parser, const char* message) {
    ErrorType type = parser->current->type == TOKEN_ERROR ? SCANNER_ERROR : PARSER_ERROR;
    addError(&parser->errors, message, *parser->current, type);
    synchronizeAfterError(parser);
}

// ---------------------------------------------------------------------------
// ------------------------------- PRODUCTIONS -------------------------------
// ---------------------------------------------------------------------------

// Forward declarations
static Expression* expression(ASTParser* parser);
static Statement* statement(ASTParser* parser);
static Expression* blockExpression(ASTParser* parser);

/**
 * Terminal rule. Match identifier token.
 */
static Literal* identifierLiteral(ASTParser* parser) {
    Token* identifier = consume(parser, TOKEN_IDENTIFIER, "Expected identifier.");
    IdentifierLiteral* identifierLiteral = allocateASTNode(IdentifierLiteral);
    identifierLiteral->token = *(identifier);

    Literal* literal = allocateASTNode(Literal);
    literal->type = IDENTIFIER_LITERAL;
    literal->as.identifierLiteral = identifierLiteral;

    return literal;
}

/**
 * Terminal rule. Match a string token.
 */
static Literal* stringLiteral(ASTParser* parser) {
    Token* string = consume(parser, TOKEN_STRING, "Expected string.");
    StringLiteral* stringLiteral = allocateASTNode(StringLiteral);
    stringLiteral->token = *(string);

    Literal* literal = allocateASTNode(Literal);
    literal->type = STRING_LITERAL;
    literal->as.stringLiteral = stringLiteral;

    return literal;
}

/**
 * Terminal rule. Match a number literal.
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

/**
 * Terminal rule. Match a boolean literal.
 */
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
 * block-expression
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
        case TOKEN_IDENTIFIER: {
            Literal* tempLiteral = identifierLiteral(parser);

            Expression* expression = allocateASTNode(Expression);
            expression->type = PRIMARY_EXPRESSION;

            PrimaryExpression* primaryExpression = allocateASTNode(PrimaryExpression);
            primaryExpression->literal = tempLiteral;

            expression->as.primaryExpression = primaryExpression;

            return expression;
            break;
        }
        case TOKEN_STRING: {
            Literal* tempLiteral = stringLiteral(parser);

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
        case TOKEN_LEFT_PAREN: {
            advance(parser);
            Expression* expr = expression(parser);
            consume(parser, TOKEN_RIGHT_PAREN, "Expected ')' after expression.");
            return expr;
            break;
        }
        case TOKEN_LEFT_CURLY: {
            Expression* expr = blockExpression(parser);
            return expr;
            break;
        }
        default: {
            reportParserErrorAndSynchronize(parser, "Expected expression.");
            return NULL;
            break;
        }
    }
}

/**
 * argument-list:
 *  expression ( "," expression )*
 */
static ExpressionArray* argumentList(ASTParser* parser) {
    ExpressionArray* arguments = (ExpressionArray*)malloc(sizeof(ExpressionArray));
    INIT_ARRAY((*arguments), Expression*);

    if (peek(parser)->type != TOKEN_RIGHT_PAREN) {
        do {
            Expression* expr = expression(parser);
            INSERT_ARRAY((*arguments), expr, Expression);
        } while (match(parser, TOKEN_COMMA));
    }
    return arguments;
}

/**
 * postfix-expression:
 *  primary-expression
 *  postfix-expression "(" ")"
 *  postfix-expression "(" argument-list ")"
 *  postfix-expression "." identifier
 *
 * Note: postfixExpression() is slightly different than other parsing functions because it can return
 * different types of AST nodes (e.g. CallExpression, MemberExpression). This is fine; Sol combines its
 * concrete tree parser and abstract syntax tree parser.
 */

static Expression* postfixExpression(ASTParser* parser) {
    Expression* expr = primaryExpression(parser);

    while (true) {
        if (match(parser, TOKEN_LEFT_PAREN)) {
            // Function call
            CallExpression* callExpr = allocateASTNode(CallExpression);
            callExpr->leftHandSide = expr;
            callExpr->arguments = argumentList(parser);

            consume(parser, TOKEN_RIGHT_PAREN, "Expected ')' after arguments.");

            expr = allocateASTNode(Expression);
            expr->type = CALL_EXPRESSION;
            expr->as.callExpression = callExpr;
        } else if (match(parser, TOKEN_DOT)) {
            // Member access
            Token* identifier = consume(parser, TOKEN_IDENTIFIER, "Expected an identifier after '.' in member expression.");

            MemberExpression* memberExpr = allocateASTNode(MemberExpression);
            memberExpr->leftHandSide = expr;

            IdentifierLiteral* identifierLiteral = allocateASTNode(IdentifierLiteral);
            identifierLiteral->token = *identifier;

            Expression* identifierExpr = allocateASTNode(Expression);
            identifierExpr->type = PRIMARY_EXPRESSION;
            identifierExpr->as.primaryExpression = allocateASTNode(PrimaryExpression);
            identifierExpr->as.primaryExpression->literal = allocateASTNode(Literal);
            identifierExpr->as.primaryExpression->literal->type = IDENTIFIER_LITERAL;
            identifierExpr->as.primaryExpression->literal->as.identifierLiteral = identifierLiteral;

            memberExpr->rightHandSide = identifierExpr;

            expr = allocateASTNode(Expression);
            expr->type = MEMBER_EXPRESSION;
            expr->as.memberExpression = memberExpr;
        } else {
            break;
        }
    }
    return expr;
}

/**
 * unary-expression:
 *  postfix-expression
 *  ( "!" )* postfix-expression
 *  ( "-" )* postfix-expression
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
    return postfixExpression(parser);
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

        leftExpression = allocateASTNode(Expression);
        leftExpression->type = LOGICAL_OR_EXPRESSION;
        leftExpression->as.logicalOrExpression = logicalOrExpression;
    }
    return leftExpression;
}

/**
 * block-expression:
 *  "{" statement* expression "}"
 */
static Expression* blockExpression(ASTParser* parser) {
    consume(parser, TOKEN_LEFT_CURLY, "Expected '{' before the start of a block expression.");
    BlockExpression* blockExpr = allocateASTNode(BlockExpression);
    blockExpr->lastExpression = NULL;
    INIT_ARRAY(blockExpr->statementArray, Statement*);

    while (!check(parser, TOKEN_EOF) && !check(parser, TOKEN_RIGHT_CURLY)) {
        // Keep track of where the parser was before this statement to potentially backtrack later.
        Token* parserPositionBeforeStatement = parser->current;

        Statement* statementNode = statement(parser);

        if (statementNode != NULL) {
            INSERT_ARRAY(blockExpr->statementArray, statementNode, Statement*);

            // If the statement we just parsed is the last one in the block, we try to
            // convert it to an expression by backtrackind and re-parsing.
            if (check(parser, TOKEN_RIGHT_CURLY)) {
                // There default case is that there's no expression at the end of the block.
                // Scala would return a Unit here; we leave it as NULL.
                Expression* lastExpression = NULL;

                Statement* lastStatementInThisBlock = statementNode;
                if (lastStatementInThisBlock != NULL) {
                    // Expression statements and block statements can be converted
                    // to expressions.
                    if (lastStatementInThisBlock->type == EXPRESSION_STATEMENT || lastStatementInThisBlock->type == BLOCK_STATEMENT) {
                        // Backtrack
                        parser->current = parserPositionBeforeStatement;
                        parser->previous = parser->current - 1;

                        // Parse expression
                        lastExpression = expression(parser);
                        checkSemicolonAfterExpression(parser, lastExpression, "Impossible. Expected ';' after block expression.");

                        // Remove the last statement since we're adding it as the last expression.
                        blockExpr->statementArray.used--;
                    }
                }
                blockExpr->lastExpression = lastExpression;
                break;
            }
        } else {
            reportParserErrorAndSynchronize(parser, "Error parsing statement in block expression.");
        }
    }
    consume(parser, TOKEN_RIGHT_CURLY, "Unclosed block expression. Expected '}'.");

    // Block expressions must have at least one expression in them.
    if (!blockExpr->statementArray.used && !blockExpr->lastExpression) reportParserErrorAndSynchronize(parser, "Encountered an empty block-expression. This isn't allowed in SolScript.");

    // Make the last ExpressionStatement the block's `lastExpression`

    Expression* expr = allocateASTNode(Expression);
    expr->type = BLOCK_EXPRESSION;
    expr->as.blockExpression = blockExpr;

    return expr;
}

/**
 * parameter-list:
 *  identifier ( "," identifier )*
 */
static IdentifierArray* parameterList(ASTParser* parser) {
    IdentifierArray* parameters = (IdentifierArray*)malloc(sizeof(IdentifierArray));
    INIT_ARRAY((*parameters), Token);

    if (peek(parser)->type == TOKEN_IDENTIFIER) {
        do {
            if (parameters->used == MAX_NUM_PARAMETERS_IN_FUNCTION) reportParserErrorAndSynchronize(parser, "Exceeded maximum number of parameters.");
            Literal* literal = identifierLiteral(parser);
            INSERT_ARRAY((*parameters), *(literal->as.identifierLiteral), IdentifierLiteral);
        } while (match(parser, TOKEN_COMMA));
    }
    return parameters;
}

/**
 * lambda-expression:
 *  "lambda" "(" ")" "{" block-expression "}"
 *  "lambda" "(" parameter-list ")" "{" block-expression "}"
 */
static Expression* lambdaExpression(ASTParser* parser) {
    consume(parser, TOKEN_LAMBDA, "Expected 'lambda' keyword in lambda expression.");

    consume(parser, TOKEN_LEFT_PAREN, "Expected '(' after 'lambda'.");
    LambdaExpression* lambdaExpr = allocateASTNode(LambdaExpression);
    lambdaExpr->parameters = parameterList(parser);
    consume(parser, TOKEN_RIGHT_PAREN, "Expected ')' after lambda parameters.");

    lambdaExpr->bodyBlock = blockExpression(parser)->as.blockExpression;

    Expression* expr = allocateASTNode(Expression);
    expr->type = LAMBDA_EXPRESSION;
    expr->as.lambdaExpression = lambdaExpr;

    return expr;
}

/**
 * struct-declaration:
 *  identifier ":" expression
 *  "prototype" ":" identifier
 */
static StructDeclaration* structDeclaration(ASTParser* parser) {
    StructDeclaration* declaration = allocateASTNode(StructDeclaration);

    if (match(parser, TOKEN_PROTOTYPE)) {
        consume(parser, TOKEN_COLON, "Expected ':' after 'prototype'.");
        declaration->isPrototype = true;
        declaration->identifier = identifierLiteral(parser)->as.identifierLiteral;
        declaration->maybeExpression = NULL;
    } else {
        declaration->isPrototype = false;
        declaration->identifier = identifierLiteral(parser)->as.identifierLiteral;
        consume(parser, TOKEN_COLON, "Expected ':' after field identifier.");
        declaration->maybeExpression = expression(parser);
    }
    consume(parser, TOKEN_SEMICOLON, "Expected semicolon after declaration in struct.");

    return declaration;
}

/**
 * struct-declaration-list:
 *  struct-declaration
 *  struct-declaration-list "," struct-declaration
 */
static StructDeclarationArray structDeclarationArray(ASTParser* parser) {
    StructDeclarationArray array;
    INIT_ARRAY(array, StructDeclaration*);

    while (!check(parser, TOKEN_RIGHT_CURLY) && !check(parser, TOKEN_EOF)) {
        StructDeclaration* declaration = structDeclaration(parser);
        INSERT_ARRAY(array, declaration, StructDeclaration*);
    }

    return array;
}

/**
 * struct-expression:
 *  "struct" "{" struct-declaration-list "}"
 */
static Expression* structExpression(ASTParser* parser) {
    consume(parser, TOKEN_STRUCT, "Expected 'struct' keyword in struct-expression.");
    consume(parser, TOKEN_LEFT_CURLY, "Expected '{' after 'struct'.");

    StructExpression* structExpr = allocateASTNode(StructExpression);
    structExpr->declarationArray = structDeclarationArray(parser);

    consume(parser, TOKEN_RIGHT_CURLY, "Expected '}' after struct declarations.");

    Expression* expr = allocateASTNode(Expression);
    expr->type = STRUCT_EXPRESSION;
    expr->as.structExpression = structExpr;

    return expr;
}

/**
 * expression:
 *  struct-expression
 *  function-expression
 *  logical-or-expression
 */
static Expression* expression(ASTParser* parser) {
    switch (peek(parser)->type) {
        case TOKEN_LAMBDA:
            return lambdaExpression(parser);
        case TOKEN_STRUCT:
            return structExpression(parser);
        default:
            return logicalOrExpression(parser);
    }
}

/**
 * val-declaration:
 *  "val" identifier "=" expression ";"
 */
static Statement* valDeclaration(ASTParser* parser) {
    consume(parser, TOKEN_VAL, "Expected 'val' in a val declaration.");
    Literal* tempIdentifier = identifierLiteral(parser);
    if (check(parser, TOKEN_ERROR)) {
        printErrorToken(parser, "Error parsing val declaration. ", *(parser->current));
    }
    consume(parser, TOKEN_EQUAL, "Expected '=' in a val declaration.");
    Expression* tempExpression = expression(parser);
    checkSemicolonAfterExpression(parser, tempExpression, "Expected ';' following the expression in val declaration.");

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
 * var-declaration:
 *  "var" identifier ";"
 *  "var" identifier "=" expression  ";"
 */
static Statement* varDeclaration(ASTParser* parser) {
    consume(parser, TOKEN_VAR, "Expected the keyword 'var' in a var declaration.");

    Literal* tempIdentifier = identifierLiteral(parser);
    if (check(parser, TOKEN_ERROR)) {
        printErrorToken(parser, "Error parsing var declaration. ", *(parser->current));
    }

    Expression* tempExpression = NULL;
    if (match(parser, TOKEN_EQUAL)) {
        tempExpression = expression(parser);
    }

    checkSemicolonAfterExpression(parser, tempExpression, "Expected ';' following the expression in var declaration.");

    VarDeclarationStatement* varDeclarationStatement = allocateASTNode(VarDeclarationStatement);
    varDeclarationStatement->identifier = tempIdentifier->as.identifierLiteral;
    varDeclarationStatement->maybeExpression = tempExpression;

    // Wrap VarDeclarationStatement in a Statement
    Statement* statement = allocateASTNode(Statement);
    statement->type = VAR_DECLARATION_STATEMENT;
    statement->as.varDeclarationStatement = varDeclarationStatement;
    return statement;
}

/**
 * assignment-statement:
 *  expression "=" expression
 *
 * (The compiler will ensures th expression is a valid target of assigment.)
 */
static Statement* assignmentStatement(ASTParser* parser, Expression* optionalExpression) {
    Expression* targetExpr = optionalExpression ? optionalExpression : expression(parser);

    consume(parser, TOKEN_EQUAL, "Expected '=' in an assignment statement.");

    Expression* valueExpr = expression(parser);
    checkSemicolonAfterExpression(parser, valueExpr, "Expected ';' after assignment statement.");

    AssignmentStatement* assignmentStmt = allocateASTNode(AssignmentStatement);
    assignmentStmt->target = targetExpr;
    assignmentStmt->value = valueExpr;

    Statement* stmt = allocateASTNode(Statement);
    stmt->type = ASSIGNMENT_STATEMENT;
    stmt->as.assignmentStatement = assignmentStmt;

    return stmt;
}

/**
 * declaration:
 *  var-declaration
 *  val-declaration
 */
static Statement* declaration(ASTParser* parser) {
    if (peek(parser)->type == TOKEN_VAL) {
        return valDeclaration(parser);
    } else if (peek(parser)->type == TOKEN_VAR) {
        return varDeclaration(parser);
    } else {
        reportParserErrorAndSynchronize(parser, "Expected 'var' or 'val' in a declaration.");  // This should be unreacheable
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

/**
 * expression-statement:
 *  expression ";"
 *
 * If an expression is passed in, it will be used to create the ExpressionStatement.
 */
static Statement* expressionStatement(ASTParser* parser, Expression* optionalExpression) {
    Expression* expr = optionalExpression ? optionalExpression : expression(parser);

    checkSemicolonAfterExpression(parser, expr, "Expected ';' after expression-statement.");

    ExpressionStatement* exprStmt = allocateASTNode(ExpressionStatement);
    exprStmt->expression = expr;

    Statement* stmt = allocateASTNode(Statement);
    stmt->type = EXPRESSION_STATEMENT;
    stmt->as.expressionStatement = exprStmt;

    return stmt;
}

/**
 * block-statement:
 *  "{" statement* "}"
 */
static Statement* blockStatement(ASTParser* parser) {
    consume(parser, TOKEN_LEFT_CURLY, "Expected '{' before the start of a block.");
    BlockStatement* blockStmt = allocateASTNode(BlockStatement);
    INIT_ARRAY(blockStmt->statementArray, Statement*);

    while (!check(parser, TOKEN_RIGHT_CURLY) && !check(parser, TOKEN_EOF)) {
        Statement* statementNode = statement(parser);

        if (statementNode != NULL) {
            INSERT_ARRAY(blockStmt->statementArray, statementNode, Statement*);
        } else {
            reportParserErrorAndSynchronize(parser, "Error parsing statement.");
        }
    }
    consume(parser, TOKEN_RIGHT_CURLY, "Unclosed block. Expected '}'.");
    match(parser, TOKEN_SEMICOLON);  // Semicolons are optional after blocks

    Statement* stmt = allocateASTNode(Statement);
    stmt->type = BLOCK_STATEMENT;
    stmt->as.blockStatement = blockStmt;

    return stmt;
}

/**
 * selection-statement:
 *  "if" "(" expression ")" statement
 *  "if" "(" expression ")" statement "else" statement
 */
static Statement* selectionStatement(ASTParser* parser) {
    consume(parser, TOKEN_IF, "Invalid State: Expected \"if\" in if statement.");
    // consume(parser, TOKEN_LEFT_PAREN, "Expected left parenthesis, \"(\", before the condition expression of an if-statement.");
    Expression* conditionExpression = expression(parser);
    // consume(parser, TOKEN_RIGHT_PAREN, "Expected right parenthesis, \")\", after the condition expression of an if-statement.");

    Statement* trueStatement = statement(parser);

    SelectionStatement* selectionStatement = allocateASTNode(SelectionStatement);
    selectionStatement->conditionExpression = conditionExpression;
    selectionStatement->trueStatement = trueStatement;

    if (match(parser, TOKEN_ELSE)) {
        Statement* falseStatement = statement(parser);
        selectionStatement->falseStatement = falseStatement;
    } else {
        selectionStatement->falseStatement = NULL;
    }

    Statement* stmt = allocateASTNode(Statement);
    stmt->type = SELECTION_STATEMENT;
    stmt->as.selectionStatement = selectionStatement;

    return stmt;
}

/**
 * iteration-statement:
 *  "while" "(" expression ")" block-statement
 */
static Statement* iterationStatement(ASTParser* parser) {
    consume(parser, TOKEN_WHILE, "InvalidStateException: Expected \"while\" in while statement.");
    Expression* conditionExpression = expression(parser);

    Statement* bodyStatement = statement(parser);

    IterationStatement* iterationStatement = allocateASTNode(IterationStatement);
    iterationStatement->conditionExpression = conditionExpression;
    iterationStatement->bodyStatement = bodyStatement;

    Statement* stmt = allocateASTNode(Statement);
    stmt->type = ITERATION_STATEMENT;
    stmt->as.iterationStatement = iterationStatement;

    return stmt;
}

/**
 * return-statement:
 *  "return" ";"
 *  "return" expression ";"
 */
static Statement* returnStatement(ASTParser* parser) {
    consume(parser, TOKEN_RETURN, "Expected 'return' keyword.");

    ReturnStatement* returnStmt = allocateASTNode(ReturnStatement);

    if (!check(parser, TOKEN_SEMICOLON)) {
        returnStmt->expression = expression(parser);
    } else {
        returnStmt->expression = NULL;
    }

    consume(parser, TOKEN_SEMICOLON, "Expected ';' after return statement.");

    Statement* stmt = allocateASTNode(Statement);
    stmt->type = RETURN_STATEMENT;
    stmt->as.returnStatement = returnStmt;

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
    TokenType currentType = peek(parser)->type;
    switch (currentType) {
        case TOKEN_VAR:
        case TOKEN_VAL:
            return declaration(parser);
        case TOKEN_PRINT:
            return printStatement(parser);
        case TOKEN_LEFT_CURLY:
            return blockStatement(parser);
        case TOKEN_WHILE:
            return iterationStatement(parser);
        case TOKEN_IF:
            return selectionStatement(parser);
        case TOKEN_RETURN:
            return returnStatement(parser);
        default: {
            // In the default case, we know there's going to be an expression.
            Expression* tempExpression = expression(parser);

            // If that expression is followed by an "=", it's the LHS of an assigment statement.
            // (I don't love having to look-ahead here, but it'll do for now.)
            if (check(parser, TOKEN_EQUAL))
                return assignmentStatement(parser, tempExpression);

            // If not, it's just an expression statement.
            return expressionStatement(parser, tempExpression);
        }
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
            reportParserErrorAndSynchronize(parser, "Exceeded maximum number of statements.");
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

    if (hadError(&parser->errors)) {
        printErrors(&parser->errors);
    }

#if DEBUG_PARSER
    printf("AST\n");
    printAST(parser->source);
#endif

    return parser->source;
}

Source* parseASTFromTokens(ASTParser* parser, TokenArray* tokenArray) {
    initASTParser(parser, *tokenArray);
    Source* source = parseAST(parser);
    return source;
}