#include "parser.h"

#include <string.h>

#include "../minunit.h"
#include "debug.h"

// Helper function to create a token array
static TokenArray createTokenArray(TokenType types[], int numTokens) {
    TokenArray array;
    INIT_ARRAY(array, Token);
    for (int i = 0; i < numTokens; i++) {
        Token token = {.type = types[i], .start = NULL, .length = 0, .lineNo = 0, .colNo = 0};
        INSERT_ARRAY(array, token, Token);
    }
    return array;
}

// Helper function to create a token
static Token createToken(TokenType type, const char* lexeme) {
    Token token = {
        .type = type,
        .start = lexeme,
        .length = strlen(lexeme),
        .lineNo = 1,
        .colNo = 1};
    return token;
}

// Test for parsing a val declaration with a simple expression
int test_parser_simpleExpression() {
    // Create a mock token array for the expression "val result = 1 + 2"
    Token tokensArray[] = {
        createToken(TOKEN_VAL, "val"),
        createToken(TOKEN_IDENTIFIER, "result"),
        createToken(TOKEN_EQUAL, "="),
        createToken(TOKEN_NUMBER, "1"),
        createToken(TOKEN_PLUS, "+"),
        createToken(TOKEN_NUMBER, "2"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = 8,
        .size = 8};

    // Initialize the parser
    ASTParser parser;
    initASTParser(&parser, tokens);

    // Parse the expression
    Source* source = parseAST(&parser);

    printAST(source);

    // Assertions to check the structure of the parsed AST
    ASSERT(source->numberOfStatements == 1);

    // Check the first statement is a val declaration
    Statement* statement = source->rootStatements[0];
    ASSERT(statement->type == VAL_DECLARATION_STATEMENT);

    // Validate the structure of the val declaration
    ValDeclarationStatement* valDecl = statement->as.valDeclarationStatement;
    ASSERT(strcmp(valDecl->identifier->token.start, "result") == 0);

    // Check the right-hand side expression of the val declaration
    ASSERT(valDecl->expression->type == ADDITIVE_EXPRESSION);

    AdditiveExpression* addExpr = valDecl->expression->as.additiveExpression;
    ASSERT(addExpr->leftExpression->type == PRIMARY_EXPRESSION);
    ASSERT(addExpr->rightExpression->type == PRIMARY_EXPRESSION);

    // Check the actual values in the number literals
    PrimaryExpression* leftPrimary = addExpr->leftExpression->as.primaryExpression;
    ASSERT(leftPrimary->literal->type == NUMBER_LITERAL);
    ASSERT(strcmp(leftPrimary->literal->as.numberLiteral->token.start, "1") == 0);

    PrimaryExpression* rightPrimary = addExpr->rightExpression->as.primaryExpression;
    ASSERT(rightPrimary->literal->type == NUMBER_LITERAL);
    ASSERT(strcmp(rightPrimary->literal->as.numberLiteral->token.start, "2") == 0);

    // Clean up
    freeSource(source);

    return SUCCESS_RETURN_CODE;
}

int test_parser_printStatement() {
    TokenType types[] = {TOKEN_PRINT, TOKEN_NUMBER, TOKEN_PLUS, TOKEN_NUMBER, TOKEN_SEMICOLON, TOKEN_EOF};
    TokenArray tokens = createTokenArray(types, 6);

    ASTParser parser;
    Source* source = parseASTFromTokens(&parser, &tokens);
    ASSERT(source->numberOfStatements == 1);
    ASSERT(source->rootStatements[0]->type == PRINT_STATEMENT);
    ASSERT(source->rootStatements[0]->as.printStatement->expression->type == ADDITIVE_EXPRESSION);

    FREE_ARRAY(tokens);
    freeSource(source);
    return SUCCESS_RETURN_CODE;
}

// TODO: fill out this test
int test_parser_errorHandling() {
    TokenType types[] = {TOKEN_NUMBER, TOKEN_PLUS, TOKEN_EOF};
    TokenArray tokens = createTokenArray(types, 3);

    ASTParser parser;
    initASTParser(&parser, tokens);

    // You should set up an environment to catch the error or simulate it
    // Then call parseAST and assert the expected behavior

    FREE_ARRAY(tokens);
    // Free other resources if necessary

    return SUCCESS_RETURN_CODE;
}

// Example function to test parsing of a logical OR expression, including detailed assertions
int test_parser_logicalOrExpression() {
    Token tokensArray[] = {
        createToken(TOKEN_TRUE, "true"),
        createToken(TOKEN_OR_OR, "||"),
        createToken(TOKEN_FALSE, "false"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = 4,
        .size = 4};

    ASTParser parser;
    Source* source = parseASTFromTokens(&parser, &tokens);
    printAST(source);

    ASSERT(source->numberOfStatements == 1);

    Statement* statement = source->rootStatements[0];
    ASSERT(statement->type == EXPRESSION_STATEMENT);

    Expression* expression = statement->as.expressionStatement->expression;
    ASSERT(expression->type == LOGICAL_OR_EXPRESSION);

    LogicalOrExpression* orExpr = expression->as.logicalOrExpression;
    ASSERT(orExpr->leftExpression->type == PRIMARY_EXPRESSION);
    ASSERT(orExpr->rightExpression->type == PRIMARY_EXPRESSION);

    // Assert the left expression is a primary expression with a literal of type TRUE
    PrimaryExpression* leftPrimary = orExpr->leftExpression->as.primaryExpression;
    ASSERT(leftPrimary->literal->type == BOOLEAN_LITERAL);
    ASSERT(strcmp(leftPrimary->literal->as.booleanLiteral->token.start, "true") == 0);

    // Assert the right expression is a primary expression with a literal of type FALSE
    PrimaryExpression* rightPrimary = orExpr->rightExpression->as.primaryExpression;
    ASSERT(rightPrimary->literal->type == BOOLEAN_LITERAL);
    ASSERT(strcmp(rightPrimary->literal->as.booleanLiteral->token.start, "false") == 0);

    freeSource(source);
    return SUCCESS_RETURN_CODE;
}

// Test for parsing a logical AND expression
int test_parser_logicalAndExpression() {
    Token tokensArray[] = {
        createToken(TOKEN_FALSE, "false"),
        createToken(TOKEN_AND_AND, "&&"),
        createToken(TOKEN_TRUE, "true"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = 4,
        .size = 4};

    ASTParser parser;
    initASTParser(&parser, tokens);
    Source* source = parseAST(&parser);
    printAST(source);

    ASSERT(source->numberOfStatements == 1);

    Statement* statement = source->rootStatements[0];
    ASSERT(statement->type == EXPRESSION_STATEMENT);

    Expression* expression = statement->as.expressionStatement->expression;
    ASSERT(expression->type == LOGICAL_AND_EXPRESSION);

    LogicalAndExpression* andExpr = expression->as.logicalAndExpression;
    ASSERT(andExpr->leftExpression->type == PRIMARY_EXPRESSION);
    ASSERT(andExpr->rightExpression->type == PRIMARY_EXPRESSION);

    PrimaryExpression* leftPrimary = andExpr->leftExpression->as.primaryExpression;
    ASSERT(leftPrimary->literal->type == BOOLEAN_LITERAL);
    ASSERT(strcmp(leftPrimary->literal->as.booleanLiteral->token.start, "false") == 0);

    PrimaryExpression* rightPrimary = andExpr->rightExpression->as.primaryExpression;
    ASSERT(rightPrimary->literal->type == BOOLEAN_LITERAL);
    ASSERT(strcmp(rightPrimary->literal->as.booleanLiteral->token.start, "true") == 0);

    freeSource(source);
    return SUCCESS_RETURN_CODE;
}

// Test for parsing an Equality Expression
int test_parser_equalityExpression() {
    Token tokensArray[] = {
        createToken(TOKEN_NUMBER, "5"),
        createToken(TOKEN_EQUAL_EQUAL, "=="),
        createToken(TOKEN_NUMBER, "5"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = 4,
        .size = 4};

    ASTParser parser;
    initASTParser(&parser, tokens);
    Source* source = parseAST(&parser);
    printAST(source);

    ASSERT(source->numberOfStatements == 1);

    Statement* statement = source->rootStatements[0];
    ASSERT(statement->type == EXPRESSION_STATEMENT);

    Expression* expression = statement->as.expressionStatement->expression;
    ASSERT(expression->type == EQUALITY_EXPRESSION);

    EqualityExpression* eqExpr = expression->as.equalityExpression;
    ASSERT(eqExpr->leftExpression->type == PRIMARY_EXPRESSION);
    ASSERT(eqExpr->rightExpression->type == PRIMARY_EXPRESSION);

    PrimaryExpression* leftPrimary = eqExpr->leftExpression->as.primaryExpression;
    ASSERT(leftPrimary->literal->type == NUMBER_LITERAL);
    ASSERT(strcmp(leftPrimary->literal->as.numberLiteral->token.start, "5") == 0);

    PrimaryExpression* rightPrimary = eqExpr->rightExpression->as.primaryExpression;
    ASSERT(rightPrimary->literal->type == NUMBER_LITERAL);
    ASSERT(strcmp(rightPrimary->literal->as.numberLiteral->token.start, "5") == 0);

    freeSource(source);
    return SUCCESS_RETURN_CODE;
}

int test_parser_comparisonExpression() {
    Token tokensArray[] = {
        createToken(TOKEN_NUMBER, "10"),
        createToken(TOKEN_GREATER, ">"),
        createToken(TOKEN_NUMBER, "5"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = 4,
        .size = 4};

    ASTParser parser;
    initASTParser(&parser, tokens);
    Source* source = parseAST(&parser);
    printAST(source);

    ASSERT(source->numberOfStatements == 1);

    Statement* statement = source->rootStatements[0];
    ASSERT(statement->type == EXPRESSION_STATEMENT);

    Expression* expression = statement->as.expressionStatement->expression;
    ASSERT(expression->type == COMPARISON_EXPRESSION);

    ComparisonExpression* compExpr = expression->as.comparisonExpression;
    ASSERT(compExpr->leftExpression->type == PRIMARY_EXPRESSION);
    ASSERT(compExpr->rightExpression->type == PRIMARY_EXPRESSION);

    PrimaryExpression* leftPrimary = compExpr->leftExpression->as.primaryExpression;
    ASSERT(leftPrimary->literal->type == NUMBER_LITERAL);
    ASSERT(strcmp(leftPrimary->literal->as.numberLiteral->token.start, "10") == 0);

    PrimaryExpression* rightPrimary = compExpr->rightExpression->as.primaryExpression;
    ASSERT(rightPrimary->literal->type == NUMBER_LITERAL);
    ASSERT(strcmp(rightPrimary->literal->as.numberLiteral->token.start, "5") == 0);

    freeSource(source);
    return SUCCESS_RETURN_CODE;
}

int test_parser_multiplicativeExpression() {
    Token tokensArray[] = {
        createToken(TOKEN_NUMBER, "3"),
        createToken(TOKEN_STAR, "*"),
        createToken(TOKEN_NUMBER, "2"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = 4,
        .size = 4};

    ASTParser parser;
    initASTParser(&parser, tokens);
    Source* source = parseAST(&parser);
    printAST(source);

    ASSERT(source->numberOfStatements == 1);

    Statement* statement = source->rootStatements[0];
    ASSERT(statement->type == EXPRESSION_STATEMENT);

    Expression* expression = statement->as.expressionStatement->expression;
    ASSERT(expression->type == MULTIPLICATIVE_EXPRESSION);

    MultiplicativeExpression* multExpr = expression->as.multiplicativeExpression;
    ASSERT(multExpr->leftExpression->type == PRIMARY_EXPRESSION);
    ASSERT(multExpr->rightExpression->type == PRIMARY_EXPRESSION);

    PrimaryExpression* leftPrimary = multExpr->leftExpression->as.primaryExpression;
    ASSERT(leftPrimary->literal->type == NUMBER_LITERAL);
    ASSERT(strcmp(leftPrimary->literal->as.numberLiteral->token.start, "3") == 0);

    PrimaryExpression* rightPrimary = multExpr->rightExpression->as.primaryExpression;
    ASSERT(rightPrimary->literal->type == NUMBER_LITERAL);
    ASSERT(strcmp(rightPrimary->literal->as.numberLiteral->token.start, "2") == 0);

    freeSource(source);
    return SUCCESS_RETURN_CODE;
}

int test_parser_unaryExpression() {
    Token tokensArray[] = {
        createToken(TOKEN_MINUS, "-"),
        createToken(TOKEN_NUMBER, "1"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = 3,
        .size = 3};

    ASTParser parser;
    initASTParser(&parser, tokens);
    Source* source = parseAST(&parser);
    printAST(source);

    ASSERT(source->numberOfStatements == 1);

    Statement* statement = source->rootStatements[0];
    ASSERT(statement->type == EXPRESSION_STATEMENT);

    Expression* expression = statement->as.expressionStatement->expression;
    ASSERT(expression->type == UNARY_EXPRESSION);

    UnaryExpression* unaryExpr = expression->as.unaryExpression;
    ASSERT(unaryExpr->rightExpression->type == PRIMARY_EXPRESSION);

    PrimaryExpression* rightPrimary = unaryExpr->rightExpression->as.primaryExpression;
    ASSERT(rightPrimary->literal->type == NUMBER_LITERAL);
    ASSERT(strcmp(rightPrimary->literal->as.numberLiteral->token.start, "1") == 0);

    freeSource(source);
    return SUCCESS_RETURN_CODE;
}

int test_parser_booleanLiteral() {
    Token tokensArray[] = {
        createToken(TOKEN_TRUE, "true"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = 2,
        .size = 2};

    ASTParser parser;
    initASTParser(&parser, tokens);
    Source* source = parseAST(&parser);
    printAST(source);

    ASSERT(source->numberOfStatements == 1);

    Statement* statement = source->rootStatements[0];
    ASSERT(statement->type == EXPRESSION_STATEMENT);

    Expression* expression = statement->as.expressionStatement->expression;
    ASSERT(expression->type == PRIMARY_EXPRESSION);

    PrimaryExpression* primaryExpr = expression->as.primaryExpression;
    ASSERT(primaryExpr->literal->type == BOOLEAN_LITERAL);
    ASSERT(strcmp(primaryExpr->literal->as.booleanLiteral->token.start, "true") == 0);

    freeSource(source);
    return SUCCESS_RETURN_CODE;
}
