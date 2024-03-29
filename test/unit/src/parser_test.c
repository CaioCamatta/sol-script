#include "parser.h"

#include <string.h>

#include "../minunit.h"
#include "debug.h"

// Helper function to create a token array based on types, with no meaningful lexeme or other info.
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

// Utility function to convert types and lexemes into a TokenArray
static TokenArray createTokenArrayFromTypesAndLexemes(TokenType types[], const char* lexemes[], size_t count) {
    TokenArray tokens;
    INIT_ARRAY(tokens, Token);
    for (size_t i = 0; i < count; i++) {
        Token token = createToken(types[i], lexemes[i]);
        INSERT_ARRAY(tokens, token, Token);
    }
    return tokens;
}

// Macro to initialize a parser, parse the AST from tokens, and print the tree
// Assumes the source token array is named `tokens`; creates a variable called `source`.
#define PARSE_TEST_AST                  \
    ASTParser parser;                   \
    initASTParser(&parser, tokens);     \
    Source* source = parseAST(&parser); \
    printAST(source);

// Test for parsing a val declaration with a simple expression
int test_parser_simple_expression() {
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

    PARSE_TEST_AST

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

int test_parser_print_statement() {
    TokenType types[] = {TOKEN_PRINT, TOKEN_NUMBER, TOKEN_PLUS, TOKEN_NUMBER, TOKEN_SEMICOLON, TOKEN_EOF};
    TokenArray tokens = createTokenArray(types, 6);

    PARSE_TEST_AST
    ASSERT(source->numberOfStatements == 1);
    ASSERT(source->rootStatements[0]->type == PRINT_STATEMENT);
    ASSERT(source->rootStatements[0]->as.printStatement->expression->type == ADDITIVE_EXPRESSION);

    FREE_ARRAY(tokens);
    freeSource(source);
    return SUCCESS_RETURN_CODE;
}

// Example function to test parsing of a logical OR expression, including detailed assertions
int test_parser_logical_or_expression() {
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

    PARSE_TEST_AST

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
int test_parser_logical_and_expression() {
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

    PARSE_TEST_AST

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
int test_parser_equality_expression() {
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

    PARSE_TEST_AST

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

int test_parser_comparison_expression() {
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

    PARSE_TEST_AST

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

int test_parser_multiplicative_expression() {
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

    PARSE_TEST_AST

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

int test_parser_unary_expression() {
    Token tokensArray[] = {
        createToken(TOKEN_MINUS, "-"),
        createToken(TOKEN_NUMBER, "1"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = 3,
        .size = 3};

    PARSE_TEST_AST

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

int test_parser_boolean_literal() {
    Token tokensArray[] = {
        createToken(TOKEN_TRUE, "true"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = 2,
        .size = 2};

    PARSE_TEST_AST

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

// Test for parsing a complex expression combining various types of expressions and literals
int test_parser_complex_expression() {
    // val result = 10 + (2 * 3) / (4 - 2) && true || false;
    Token tokensArray[] = {
        createToken(TOKEN_VAL, "val"),
        createToken(TOKEN_IDENTIFIER, "result"),
        createToken(TOKEN_EQUAL, "="),
        createToken(TOKEN_NUMBER, "10"),
        createToken(TOKEN_PLUS, "+"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_NUMBER, "2"),
        createToken(TOKEN_STAR, "*"),
        createToken(TOKEN_NUMBER, "3"),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_SLASH, "/"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_NUMBER, "4"),
        createToken(TOKEN_MINUS, "-"),
        createToken(TOKEN_NUMBER, "2"),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_AND_AND, "&&"),
        createToken(TOKEN_TRUE, "true"),
        createToken(TOKEN_OR_OR, "||"),
        createToken(TOKEN_FALSE, "false"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = 21,
        .size = 21};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 1);

    Statement* statement = source->rootStatements[0];
    ASSERT(statement->type == VAL_DECLARATION_STATEMENT);

    ValDeclarationStatement* valDecl = statement->as.valDeclarationStatement;
    ASSERT(strcmp(valDecl->identifier->token.start, "result") == 0);

    Expression* expression = valDecl->expression;
    ASSERT(expression->type == LOGICAL_OR_EXPRESSION);

    LogicalOrExpression* orExpr = expression->as.logicalOrExpression;
    ASSERT(orExpr->leftExpression->type == LOGICAL_AND_EXPRESSION);
    ASSERT(orExpr->rightExpression->type == PRIMARY_EXPRESSION);

    // Check the right side of the OR expression is a BooleanLiteral with the value false
    PrimaryExpression* rightPrimary = orExpr->rightExpression->as.primaryExpression;
    ASSERT(rightPrimary->literal->type == BOOLEAN_LITERAL);
    ASSERT(strcmp(rightPrimary->literal->as.booleanLiteral->token.start, "false") == 0);

    // Drill down into the left side of the OR expression, which is a LogicalAndExpression
    LogicalAndExpression* andExpr = orExpr->leftExpression->as.logicalAndExpression;
    ASSERT(andExpr->leftExpression->type == ADDITIVE_EXPRESSION);
    ASSERT(andExpr->rightExpression->type == PRIMARY_EXPRESSION);

    // Check the right side of the AND expression is a BooleanLiteral with the value true
    PrimaryExpression* andRightPrimary = andExpr->rightExpression->as.primaryExpression;
    ASSERT(andRightPrimary->literal->type == BOOLEAN_LITERAL);
    ASSERT(strcmp(andRightPrimary->literal->as.booleanLiteral->token.start, "true") == 0);

    // Now, inspect the left side of the AND expression, which is an AdditiveExpression
    AdditiveExpression* addExpr = andExpr->leftExpression->as.additiveExpression;
    ASSERT(addExpr->leftExpression->type == PRIMARY_EXPRESSION);

    // Inspect the right side of the AdditiveExpression, which should be a Division expression
    Expression* divisionExpr = addExpr->rightExpression;
    ASSERT(divisionExpr->type == MULTIPLICATIVE_EXPRESSION);
    MultiplicativeExpression* divExpr = divisionExpr->as.multiplicativeExpression;
    ASSERT(divExpr->punctuator.type == TOKEN_SLASH);

    // Check the left and right expressions of the division operation
    // Left side should be a MultiplicativeExpression resulting from (2 * 3)
    MultiplicativeExpression* multiplicationExpr = divExpr->leftExpression->as.multiplicativeExpression;
    PrimaryExpression* multLeftPrimary = multiplicationExpr->leftExpression->as.primaryExpression;
    ASSERT(multLeftPrimary->literal->type == NUMBER_LITERAL);
    ASSERT(strcmp(multLeftPrimary->literal->as.numberLiteral->token.start, "2") == 0);

    PrimaryExpression* multRightPrimary = multiplicationExpr->rightExpression->as.primaryExpression;
    ASSERT(multRightPrimary->literal->type == NUMBER_LITERAL);
    ASSERT(strcmp(multRightPrimary->literal->as.numberLiteral->token.start, "3") == 0);

    // Right side should be an AdditiveExpression resulting from (4 - 2)
    AdditiveExpression* subtractionExpr = divExpr->rightExpression->as.additiveExpression;
    PrimaryExpression* subLeftPrimary = subtractionExpr->leftExpression->as.primaryExpression;
    ASSERT(subLeftPrimary->literal->type == NUMBER_LITERAL);
    ASSERT(strcmp(subLeftPrimary->literal->as.numberLiteral->token.start, "4") == 0);

    PrimaryExpression* subRightPrimary = subtractionExpr->rightExpression->as.primaryExpression;
    ASSERT(subRightPrimary->literal->type == NUMBER_LITERAL);
    ASSERT(strcmp(subRightPrimary->literal->as.numberLiteral->token.start, "2") == 0);

    freeSource(source);
    return SUCCESS_RETURN_CODE;
}

int test_parser_nested_parentheses_expression() {
    // (1 + (2 * 3));
    Token tokensArray[] = {
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_NUMBER, "1"),
        createToken(TOKEN_PLUS, "+"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_NUMBER, "2"),
        createToken(TOKEN_STAR, "*"),
        createToken(TOKEN_NUMBER, "3"),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = 11,
        .size = 11};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 1);  // Ensure one statement was parsed

    Statement* statement = source->rootStatements[0];
    ASSERT(statement->type == EXPRESSION_STATEMENT);  // Ensure the statement is an expression

    Expression* expression = statement->as.expressionStatement->expression;
    ASSERT(expression->type == ADDITIVE_EXPRESSION);  // Outermost expression should be addition

    // Left expression of the addition should be a number literal
    Expression* leftExpression = expression->as.additiveExpression->leftExpression;
    ASSERT(leftExpression->type == PRIMARY_EXPRESSION);
    ASSERT(leftExpression->as.primaryExpression->literal->type == NUMBER_LITERAL);
    ASSERT(strcmp(leftExpression->as.primaryExpression->literal->as.numberLiteral->token.start, "1") == 0);

    // Right expression of the addition should be the nested expression
    Expression* rightExpression = expression->as.additiveExpression->rightExpression;
    ASSERT(rightExpression->type == MULTIPLICATIVE_EXPRESSION);  // The nested expression should be multiplication

    // Cleanup
    freeSource(source);

    return SUCCESS_RETURN_CODE;
}

int test_parser_variable_declaration_and_reading() {
    // Manually create tokens array
    Token tokensArray[] = {
        createToken(TOKEN_VAL, "val"),
        createToken(TOKEN_IDENTIFIER, "x"),
        createToken(TOKEN_EQUAL, "="),
        createToken(TOKEN_NUMBER, "10"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_PRINT, "print"),
        createToken(TOKEN_IDENTIFIER, "x"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    // Manually initialize TokenArray
    TokenArray tokens;
    INIT_ARRAY(tokens, Token);
    for (int i = 0; i < sizeof(tokensArray) / sizeof(Token); ++i) {
        INSERT_ARRAY(tokens, tokensArray[i], Token);
    }

    PARSE_TEST_AST

    // Assertions to validate the AST structure
    ASSERT(source->numberOfStatements == 2);
    ASSERT(source->rootStatements[0]->type == VAL_DECLARATION_STATEMENT);
    ASSERT(source->rootStatements[1]->type == PRINT_STATEMENT);

    // Check that the indentifier is 'x'
    ValDeclarationStatement* valDecl = source->rootStatements[0]->as.valDeclarationStatement;
    IdentifierLiteral* idLiteral = valDecl->identifier;
    ASSERT(strncmp(idLiteral->token.start, "x", idLiteral->token.length) == 0);

    // Check that the number is correct
    NumberLiteral* numberLiteral = valDecl->expression->as.primaryExpression->literal->as.numberLiteral;
    char numberStr[10];
    snprintf(numberStr, sizeof(numberStr), "%.*s", numberLiteral->token.length, numberLiteral->token.start);
    ASSERT(strcmp(numberStr, "10") == 0);

    // Check that 'x' is in the print statemetn
    IdentifierLiteral* printIdLiteral = source->rootStatements[1]->as.printStatement->expression->as.primaryExpression->literal->as.identifierLiteral;
    ASSERT(strncmp(printIdLiteral->token.start, "x", printIdLiteral->token.length) == 0);

    // Cleanup
    freeSource(source);
    FREE_ARRAY(tokens);

    return SUCCESS_RETURN_CODE;
}

int test_parser_string_literal() {
    TokenType types[] = {TOKEN_STRING, TOKEN_SEMICOLON, TOKEN_EOF};
    const char* lexemes[] = {"\"Hello, World!\"", ";", ""};

    TokenArray tokens;
    INIT_ARRAY(tokens, Token);

    for (int i = 0; i < sizeof(types) / sizeof(TokenType); ++i) {
        Token token = createToken(types[i], lexemes[i]);
        INSERT_ARRAY(tokens, token, Token);
    }

    PARSE_TEST_AST

    // Check that the AST correctly represents a string literal expression
    ASSERT(source->numberOfStatements == 1);
    Statement* statement = source->rootStatements[0];
    ASSERT(statement->type == EXPRESSION_STATEMENT);

    Expression* expression = statement->as.expressionStatement->expression;
    ASSERT(expression->type == PRIMARY_EXPRESSION);

    PrimaryExpression* primaryExpression = expression->as.primaryExpression;
    ASSERT(primaryExpression->literal->type == STRING_LITERAL);
    ASSERT(strcmp(primaryExpression->literal->as.stringLiteral->token.start, "\"Hello, World!\"") == 0);

    // Cleanup
    freeSource(source);
    FREE_ARRAY(tokens);

    return SUCCESS_RETURN_CODE;
}

int test_parser_block_statement() {
    // Define the tokens representing a block statement with a val declaration and a print statement
    TokenType types[] = {
        TOKEN_LEFT_CURLY,                                                         // {
        TOKEN_VAL, TOKEN_IDENTIFIER, TOKEN_EQUAL, TOKEN_NUMBER, TOKEN_SEMICOLON,  // val x = 10;
        TOKEN_PRINT, TOKEN_IDENTIFIER, TOKEN_SEMICOLON,                           // print x;
        TOKEN_RIGHT_CURLY,                                                        // }
        TOKEN_EOF};
    TokenArray tokens = createTokenArray(types, sizeof(types) / sizeof(TokenType));

    // Initialize the parser and parse the AST from the tokens
    PARSE_TEST_AST

    // Assertions to verify the structure of the parsed AST
    // Expect one statement at the root, which is a block statement
    ASSERT(source->numberOfStatements == 1);
    ASSERT(source->rootStatements[0]->type == BLOCK_STATEMENT);

    // Verify the block contains two statements: a val declaration and a print statement
    BlockStatement* blockStmt = source->rootStatements[0]->as.blockStatement;
    ASSERT(blockStmt->statementArray.used == 2);

    // Verify the first statement is a val declaration
    Statement* firstStmt = blockStmt->statementArray.values[0];
    ASSERT(firstStmt->type == VAL_DECLARATION_STATEMENT);
    ValDeclarationStatement* valDecl = firstStmt->as.valDeclarationStatement;
    ASSERT(valDecl->expression->type == PRIMARY_EXPRESSION);

    // Verify the second statement is a print statement
    Statement* secondStmt = blockStmt->statementArray.values[1];
    ASSERT(secondStmt->type == PRINT_STATEMENT);
    PrintStatement* printStmt = secondStmt->as.printStatement;
    ASSERT(printStmt->expression->type == PRIMARY_EXPRESSION);

    // Cleanup
    freeSource(source);
    FREE_ARRAY(tokens);
    return SUCCESS_RETURN_CODE;
}

int test_parser_if_statement_true_branch_only() {
    TokenType types[] = {TOKEN_IF, TOKEN_LEFT_PAREN, TOKEN_TRUE, TOKEN_RIGHT_PAREN, TOKEN_LEFT_CURLY, TOKEN_PRINT, TOKEN_STRING, TOKEN_SEMICOLON, TOKEN_RIGHT_CURLY, TOKEN_EOF};
    const char* lexemes[] = {"if", "(", "true", ")", "{", "print", "\"Hello, World!\"", ";", "}", ""};

    TokenArray tokens = createTokenArrayFromTypesAndLexemes(types, lexemes, sizeof(types) / sizeof(TokenType));

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 1);
    ASSERT(source->rootStatements[0]->type == SELECTION_STATEMENT);
    ASSERT(source->rootStatements[0]->as.selectionStatement->conditionExpression->type == PRIMARY_EXPRESSION);
    ASSERT(source->rootStatements[0]->as.selectionStatement->trueStatement != NULL);
    ASSERT(source->rootStatements[0]->as.selectionStatement->falseStatement == NULL);

    ASSERT(source->rootStatements[0]->as.selectionStatement->conditionExpression->as.primaryExpression->literal->type == BOOLEAN_LITERAL);

    BlockStatement* trueBlockStmt = source->rootStatements[0]->as.selectionStatement->trueStatement->as.blockStatement;
    ASSERT(trueBlockStmt->statementArray.used == 1);
    PrintStatement* printStmt = trueBlockStmt->statementArray.values[0]->as.printStatement;
    ASSERT(printStmt->expression->type == PRIMARY_EXPRESSION);
    ASSERT(printStmt->expression->as.primaryExpression->literal->type == STRING_LITERAL);
    ASSERT(strcmp(printStmt->expression->as.primaryExpression->literal->as.stringLiteral->token.start, "\"Hello, World!\"") == 0);

    freeSource(source);
    FREE_ARRAY(tokens);
    return SUCCESS_RETURN_CODE;
}

int test_parser_if_statement_with_else_branch() {
    TokenType types[] = {TOKEN_IF, TOKEN_LEFT_PAREN, TOKEN_TRUE, TOKEN_RIGHT_PAREN, TOKEN_LEFT_CURLY, TOKEN_PRINT, TOKEN_STRING, TOKEN_SEMICOLON, TOKEN_RIGHT_CURLY, TOKEN_ELSE, TOKEN_LEFT_CURLY, TOKEN_PRINT, TOKEN_STRING, TOKEN_SEMICOLON, TOKEN_RIGHT_CURLY, TOKEN_EOF};
    const char* lexemes[] = {"if", "(", "true", ")", "{", "print", "\"Hello, World!\"", ";", "}", "else", "{", "print", "\"Goodbye, World!\"", ";", "}", ""};

    TokenArray tokens = createTokenArrayFromTypesAndLexemes(types, lexemes, sizeof(types) / sizeof(TokenType));

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 1);
    ASSERT(source->rootStatements[0]->type == SELECTION_STATEMENT);
    ASSERT(source->rootStatements[0]->as.selectionStatement->conditionExpression->type == PRIMARY_EXPRESSION);
    ASSERT(source->rootStatements[0]->as.selectionStatement->trueStatement != NULL);
    ASSERT(source->rootStatements[0]->as.selectionStatement->falseStatement != NULL);

    ASSERT(source->rootStatements[0]->as.selectionStatement->conditionExpression->as.primaryExpression->literal->type == BOOLEAN_LITERAL);

    BlockStatement* trueBlock = source->rootStatements[0]->as.selectionStatement->trueStatement->as.blockStatement;
    ASSERT(trueBlock->statementArray.used == 1);
    PrintStatement* truePrintStmt = trueBlock->statementArray.values[0]->as.printStatement;
    ASSERT(strcmp(truePrintStmt->expression->as.primaryExpression->literal->as.stringLiteral->token.start, "\"Hello, World!\"") == 0);

    BlockStatement* falseBlock = source->rootStatements[0]->as.selectionStatement->falseStatement->as.blockStatement;
    ASSERT(falseBlock->statementArray.used == 1);
    PrintStatement* falsePrintStmt = falseBlock->statementArray.values[0]->as.printStatement;
    ASSERT(strcmp(falsePrintStmt->expression->as.primaryExpression->literal->as.stringLiteral->token.start, "\"Goodbye, World!\"") == 0);

    freeSource(source);
    FREE_ARRAY(tokens);
    return SUCCESS_RETURN_CODE;
}