#include "parser.h"

#include <string.h>

#include "../../minunit.h"
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
    freeParserButNotAST(&parser);

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

    freeParserButNotAST(&parser);
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

    freeParserButNotAST(&parser);
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

    freeParserButNotAST(&parser);
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

    freeParserButNotAST(&parser);
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

    freeParserButNotAST(&parser);
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

    freeParserButNotAST(&parser);
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

    freeParserButNotAST(&parser);
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

    freeParserButNotAST(&parser);
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

    freeParserButNotAST(&parser);
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

    freeParserButNotAST(&parser);

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

    freeParserButNotAST(&parser);
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

    freeParserButNotAST(&parser);
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

    freeParserButNotAST(&parser);
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

    freeParserButNotAST(&parser);
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

    freeParserButNotAST(&parser);
    FREE_ARRAY(tokens);
    return SUCCESS_RETURN_CODE;
}

int test_parser_block_expression_simple() {
    // val a = { 3; };
    Token tokensArray[] = {
        createToken(TOKEN_VAL, "val"),
        createToken(TOKEN_IDENTIFIER, "a"),
        createToken(TOKEN_EQUAL, "="),
        createToken(TOKEN_LEFT_CURLY, "{"),
        createToken(TOKEN_NUMBER, "3"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_RIGHT_CURLY, "}"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = 9,
        .size = 9};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 1);
    Statement* statement = source->rootStatements[0];
    ASSERT(statement->type == VAL_DECLARATION_STATEMENT);

    ValDeclarationStatement* valDecl = statement->as.valDeclarationStatement;
    ASSERT(valDecl->expression->type == BLOCK_EXPRESSION);

    BlockExpression* blockExpr = valDecl->expression->as.blockExpression;
    ASSERT(blockExpr->lastExpression->type == PRIMARY_EXPRESSION);

    ASSERT(blockExpr->lastExpression->as.primaryExpression->literal->type == NUMBER_LITERAL);
    ASSERT(strcmp(blockExpr->lastExpression->as.primaryExpression->literal->as.numberLiteral->token.start, "3") == 0);

    freeParserButNotAST(&parser);
    return SUCCESS_RETURN_CODE;
}

int test_parser_block_expression_nested() {
    // val a = {{{ 3; };}}
    Token tokensArray[] = {
        createToken(TOKEN_VAL, "val"),
        createToken(TOKEN_IDENTIFIER, "a"),
        createToken(TOKEN_EQUAL, "="),
        createToken(TOKEN_LEFT_CURLY, "{"),
        createToken(TOKEN_LEFT_CURLY, "{"),
        createToken(TOKEN_LEFT_CURLY, "{"),
        createToken(TOKEN_NUMBER, "3"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_RIGHT_CURLY, "}"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_RIGHT_CURLY, "}"),
        createToken(TOKEN_RIGHT_CURLY, "}"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = 14,
        .size = 14};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 1);
    Statement* statement = source->rootStatements[0];
    ASSERT(statement->type == VAL_DECLARATION_STATEMENT);

    ValDeclarationStatement* valDecl = statement->as.valDeclarationStatement;
    ASSERT(valDecl->expression->type == BLOCK_EXPRESSION);

    BlockExpression* outerBlockExpr = valDecl->expression->as.blockExpression;
    ASSERT(outerBlockExpr->lastExpression->type == BLOCK_EXPRESSION);

    BlockExpression* middleBlockExpr = outerBlockExpr->lastExpression->as.blockExpression;
    ASSERT(middleBlockExpr->lastExpression->type == BLOCK_EXPRESSION);

    BlockExpression* innerBlockExpr = middleBlockExpr->lastExpression->as.blockExpression;
    ASSERT(innerBlockExpr->lastExpression->type == PRIMARY_EXPRESSION);

    ASSERT(innerBlockExpr->lastExpression->as.primaryExpression->literal->type == NUMBER_LITERAL);
    ASSERT(strcmp(innerBlockExpr->lastExpression->as.primaryExpression->literal->as.numberLiteral->token.start, "3") == 0);

    freeParserButNotAST(&parser);
    return SUCCESS_RETURN_CODE;
}

int test_parser_block_expression_with_statements() {
    // val a = { val b = 2; 3; };
    Token tokensArray[] = {
        createToken(TOKEN_VAL, "val"),
        createToken(TOKEN_IDENTIFIER, "a"),
        createToken(TOKEN_EQUAL, "="),
        createToken(TOKEN_LEFT_CURLY, "{"),
        createToken(TOKEN_VAL, "val"),
        createToken(TOKEN_IDENTIFIER, "b"),
        createToken(TOKEN_EQUAL, "="),
        createToken(TOKEN_NUMBER, "2"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_NUMBER, "3"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_RIGHT_CURLY, "}"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = 14,
        .size = 14};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 1);
    Statement* statement = source->rootStatements[0];
    ASSERT(statement->type == VAL_DECLARATION_STATEMENT);

    ValDeclarationStatement* valDecl = statement->as.valDeclarationStatement;
    ASSERT(valDecl->expression->type == BLOCK_EXPRESSION);

    BlockExpression* blockExpr = valDecl->expression->as.blockExpression;
    ASSERT(blockExpr->statementArray.used == 1);
    ASSERT(blockExpr->statementArray.values[0]->type == VAL_DECLARATION_STATEMENT);

    ASSERT(blockExpr->lastExpression->type == PRIMARY_EXPRESSION);
    ASSERT(blockExpr->lastExpression->as.primaryExpression->literal->type == NUMBER_LITERAL);
    ASSERT(strcmp(blockExpr->lastExpression->as.primaryExpression->literal->as.numberLiteral->token.start, "3") == 0);

    freeParserButNotAST(&parser);
    return SUCCESS_RETURN_CODE;
}

int test_parser_block_expression_as_if_condition() {
    // if ({ val a = 1; a > 0; }) {
    //     print "Passed";
    // }
    Token tokensArray[] = {
        createToken(TOKEN_IF, "if"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_LEFT_CURLY, "{"),
        createToken(TOKEN_VAL, "val"),
        createToken(TOKEN_IDENTIFIER, "a"),
        createToken(TOKEN_EQUAL, "="),
        createToken(TOKEN_NUMBER, "1"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_IDENTIFIER, "a"),
        createToken(TOKEN_GREATER, ">"),
        createToken(TOKEN_NUMBER, "0"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_RIGHT_CURLY, "}"),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_LEFT_CURLY, "{"),
        createToken(TOKEN_PRINT, "print"),
        createToken(TOKEN_STRING, "\"Passed\""),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_RIGHT_CURLY, "}"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = 20,
        .size = 20};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 1);
    Statement* statement = source->rootStatements[0];
    ASSERT(statement->type == SELECTION_STATEMENT);

    SelectionStatement* selectionStmt = statement->as.selectionStatement;
    ASSERT(selectionStmt->conditionExpression->type == BLOCK_EXPRESSION);

    BlockExpression* blockExpr = selectionStmt->conditionExpression->as.blockExpression;
    ASSERT(blockExpr->statementArray.used == 1);
    ASSERT(blockExpr->statementArray.values[0]->type == VAL_DECLARATION_STATEMENT);

    ASSERT(blockExpr->lastExpression->type == COMPARISON_EXPRESSION);
    ComparisonExpression* comparisonExpr = blockExpr->lastExpression->as.comparisonExpression;
    ASSERT(comparisonExpr->leftExpression->type == PRIMARY_EXPRESSION);
    ASSERT(comparisonExpr->leftExpression->as.primaryExpression->literal->type == IDENTIFIER_LITERAL);
    ASSERT(strcmp(comparisonExpr->leftExpression->as.primaryExpression->literal->as.identifierLiteral->token.start, "a") == 0);

    ASSERT(comparisonExpr->rightExpression->type == PRIMARY_EXPRESSION);
    ASSERT(comparisonExpr->rightExpression->as.primaryExpression->literal->type == NUMBER_LITERAL);
    ASSERT(strcmp(comparisonExpr->rightExpression->as.primaryExpression->literal->as.numberLiteral->token.start, "0") == 0);

    ASSERT(selectionStmt->trueStatement->type == BLOCK_STATEMENT);
    BlockStatement* trueBlock = selectionStmt->trueStatement->as.blockStatement;
    ASSERT(trueBlock->statementArray.used == 1);
    ASSERT(trueBlock->statementArray.values[0]->type == PRINT_STATEMENT);

    freeParserButNotAST(&parser);
    return SUCCESS_RETURN_CODE;
}

int test_parser_var_declaration() {
    // var a;
    Token tokensArray[] = {
        createToken(TOKEN_VAR, "var"),
        createToken(TOKEN_IDENTIFIER, "a"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = 4,
        .size = 4};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 1);
    Statement* statement = source->rootStatements[0];
    ASSERT(statement->type == VAR_DECLARATION_STATEMENT);

    VarDeclarationStatement* varDecl = statement->as.varDeclarationStatement;
    ASSERT(strcmp(varDecl->identifier->token.start, "a") == 0);
    ASSERT(varDecl->maybeExpression == NULL);

    freeParserButNotAST(&parser);
    return SUCCESS_RETURN_CODE;
}

int test_parser_var_declaration_with_initializer() {
    // var a = 42;
    Token tokensArray[] = {
        createToken(TOKEN_VAR, "var"),
        createToken(TOKEN_IDENTIFIER, "a"),
        createToken(TOKEN_EQUAL, "="),
        createToken(TOKEN_NUMBER, "42"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = 6,
        .size = 6};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 1);
    Statement* statement = source->rootStatements[0];
    ASSERT(statement->type == VAR_DECLARATION_STATEMENT);

    VarDeclarationStatement* varDecl = statement->as.varDeclarationStatement;
    ASSERT(strcmp(varDecl->identifier->token.start, "a") == 0);
    ASSERT(varDecl->maybeExpression != NULL);
    ASSERT(varDecl->maybeExpression->type == PRIMARY_EXPRESSION);
    ASSERT(varDecl->maybeExpression->as.primaryExpression->literal->type == NUMBER_LITERAL);
    ASSERT(strcmp(varDecl->maybeExpression->as.primaryExpression->literal->as.numberLiteral->token.start, "42") == 0);

    freeParserButNotAST(&parser);
    return SUCCESS_RETURN_CODE;
}

int test_parser_assignment() {
    // a = 2;
    Token tokensArray[] = {
        createToken(TOKEN_IDENTIFIER, "a"),
        createToken(TOKEN_EQUAL, "="),
        createToken(TOKEN_NUMBER, "2"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = 5,
        .size = 5};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 1);
    Statement* statement = source->rootStatements[0];
    ASSERT(statement->type == ASSIGNMENT_STATEMENT);

    AssignmentStatement* assignmentStmt = statement->as.assignmentStatement;
    ASSERT(assignmentStmt->target->type == PRIMARY_EXPRESSION);
    ASSERT(assignmentStmt->target->as.primaryExpression->literal->type == IDENTIFIER_LITERAL);
    ASSERT(strcmp(assignmentStmt->target->as.primaryExpression->literal->as.identifierLiteral->token.start, "a") == 0);

    ASSERT(assignmentStmt->value->type == PRIMARY_EXPRESSION);
    ASSERT(assignmentStmt->value->as.primaryExpression->literal->type == NUMBER_LITERAL);
    ASSERT(strcmp(assignmentStmt->value->as.primaryExpression->literal->as.numberLiteral->token.start, "2") == 0);

    freeParserButNotAST(&parser);
    return SUCCESS_RETURN_CODE;
}

int test_parser_iteration_statement_no_curlys() {
    // while true print 3;
    Token tokensArray[] = {
        createToken(TOKEN_WHILE, "while"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_TRUE, "true"),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_PRINT, "print"),
        createToken(TOKEN_NUMBER, "3"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = sizeof(tokensArray) / sizeof(Token),
        .size = sizeof(tokensArray) / sizeof(Token)};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 1);
    Statement* iterationStmt = source->rootStatements[0];
    ASSERT(iterationStmt->type == ITERATION_STATEMENT);

    IterationStatement* iterStmt = iterationStmt->as.iterationStatement;
    ASSERT(iterStmt->conditionExpression->type == PRIMARY_EXPRESSION);
    ASSERT(iterStmt->bodyStatement->type == PRINT_STATEMENT);

    freeParserButNotAST(&parser);
    return SUCCESS_RETURN_CODE;
}

int test_parser_iteration_statement_no_parentheses_no_curlys() {
    // while true print 3;
    Token tokensArray[] = {
        createToken(TOKEN_WHILE, "while"),
        createToken(TOKEN_TRUE, "true"),
        createToken(TOKEN_PRINT, "print"),
        createToken(TOKEN_NUMBER, "3"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = sizeof(tokensArray) / sizeof(Token),
        .size = sizeof(tokensArray) / sizeof(Token)};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 1);
    Statement* iterationStmt = source->rootStatements[0];
    ASSERT(iterationStmt->type == ITERATION_STATEMENT);

    IterationStatement* iterStmt = iterationStmt->as.iterationStatement;
    ASSERT(iterStmt->conditionExpression->type == PRIMARY_EXPRESSION);
    ASSERT(iterStmt->bodyStatement->type == PRINT_STATEMENT);

    freeParserButNotAST(&parser);
    return SUCCESS_RETURN_CODE;
}

int test_parser_iteration_statement_with_block() {
    // while (true) {print 3; print 4;};
    Token tokensArray[] = {
        createToken(TOKEN_WHILE, "while"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_TRUE, "true"),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_LEFT_CURLY, "{"),
        createToken(TOKEN_PRINT, "print"),
        createToken(TOKEN_NUMBER, "3"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_PRINT, "print"),
        createToken(TOKEN_NUMBER, "4"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_RIGHT_CURLY, "}"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = sizeof(tokensArray) / sizeof(Token),
        .size = sizeof(tokensArray) / sizeof(Token)};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 1);
    Statement* iterationStmt = source->rootStatements[0];
    ASSERT(iterationStmt->type == ITERATION_STATEMENT);

    IterationStatement* iterStmt = iterationStmt->as.iterationStatement;
    ASSERT(iterStmt->conditionExpression->type == PRIMARY_EXPRESSION);
    ASSERT(iterStmt->bodyStatement->type == BLOCK_STATEMENT);
    ASSERT(iterStmt->bodyStatement->as.blockStatement->statementArray.used == 2);

    freeParserButNotAST(&parser);
    return SUCCESS_RETURN_CODE;
}

int test_parser_lambda_expression_no_parameters() {
    // "lambda () { 42; };"
    Token tokensArray[] = {
        createToken(TOKEN_LAMBDA, "lambda"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_LEFT_CURLY, "{"),
        createToken(TOKEN_NUMBER, "42"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_RIGHT_CURLY, "}"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = 9,
        .size = 9};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 1);
    Statement* statement = source->rootStatements[0];
    ASSERT(statement->type == EXPRESSION_STATEMENT);

    Expression* expression = statement->as.expressionStatement->expression;
    ASSERT(expression->type == LAMBDA_EXPRESSION);

    LambdaExpression* lambdaExpr = expression->as.lambdaExpression;
    ASSERT(lambdaExpr->parameters->used == 0);  // No parameters

    ASSERT(lambdaExpr->bodyBlock->statementArray.used == 0);
    ASSERT(lambdaExpr->bodyBlock->lastExpression->type == PRIMARY_EXPRESSION);
    ASSERT(lambdaExpr->bodyBlock->lastExpression->as.primaryExpression->literal->type == NUMBER_LITERAL);
    ASSERT(strcmp(lambdaExpr->bodyBlock->lastExpression->as.primaryExpression->literal->as.numberLiteral->token.start, "42") == 0);

    freeParserButNotAST(&parser);
    return SUCCESS_RETURN_CODE;
}

int test_parser_lambda_expression_single_parameter() {
    // "lambda (x) { x; };"
    Token tokensArray[] = {
        createToken(TOKEN_LAMBDA, "lambda"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_IDENTIFIER, "x"),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_LEFT_CURLY, "{"),
        createToken(TOKEN_IDENTIFIER, "x"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_RIGHT_CURLY, "}"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = 10,
        .size = 10};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 1);
    Statement* statement = source->rootStatements[0];
    ASSERT(statement->type == EXPRESSION_STATEMENT);

    Expression* expression = statement->as.expressionStatement->expression;
    ASSERT(expression->type == LAMBDA_EXPRESSION);

    LambdaExpression* lambdaExpr = expression->as.lambdaExpression;
    ASSERT(lambdaExpr->parameters->used == 1);
    ASSERT(strcmp(lambdaExpr->parameters->values[0].token.start, "x") == 0);

    ASSERT(lambdaExpr->bodyBlock->statementArray.used == 0);
    ASSERT(lambdaExpr->bodyBlock->lastExpression->type == PRIMARY_EXPRESSION);
    ASSERT(lambdaExpr->bodyBlock->lastExpression->as.primaryExpression->literal->type == IDENTIFIER_LITERAL);
    ASSERT(strcmp(lambdaExpr->bodyBlock->lastExpression->as.primaryExpression->literal->as.identifierLiteral->token.start, "x") == 0);

    freeParserButNotAST(&parser);
    return SUCCESS_RETURN_CODE;
}

int test_parser_lambda_expression_multiple_parameters() {
    // "lambda (x,y) { x + y; };"
    Token tokensArray[] = {
        createToken(TOKEN_LAMBDA, "lambda"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_IDENTIFIER, "x"),
        createToken(TOKEN_COMMA, ","),
        createToken(TOKEN_IDENTIFIER, "y"),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_LEFT_CURLY, "{"),
        createToken(TOKEN_IDENTIFIER, "x"),
        createToken(TOKEN_PLUS, "+"),
        createToken(TOKEN_IDENTIFIER, "y"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_RIGHT_CURLY, "}"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = 14,
        .size = 14};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 1);
    Statement* statement = source->rootStatements[0];
    ASSERT(statement->type == EXPRESSION_STATEMENT);

    Expression* expression = statement->as.expressionStatement->expression;
    ASSERT(expression->type == LAMBDA_EXPRESSION);

    LambdaExpression* lambdaExpr = expression->as.lambdaExpression;
    ASSERT(lambdaExpr->parameters->used == 2);
    ASSERT(strcmp(lambdaExpr->parameters->values[0].token.start, "x") == 0);
    ASSERT(strcmp(lambdaExpr->parameters->values[1].token.start, "y") == 0);

    ASSERT(lambdaExpr->bodyBlock->statementArray.used == 0);
    ASSERT(lambdaExpr->bodyBlock->lastExpression->type == ADDITIVE_EXPRESSION);

    freeParserButNotAST(&parser);
    return SUCCESS_RETURN_CODE;
}

int test_parser_lambda_expression_no_last_expression_in_block() {
    // "lambda () { print "Hello"; };"
    Token tokensArray[] = {
        createToken(TOKEN_LAMBDA, "lambda"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_LEFT_CURLY, "{"),
        createToken(TOKEN_PRINT, "print"),
        createToken(TOKEN_STRING, "\"Hello\""),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_RIGHT_CURLY, "}"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = 12,
        .size = 12};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 1);
    Statement* statement = source->rootStatements[0];
    ASSERT(statement->type == EXPRESSION_STATEMENT);

    Expression* expression = statement->as.expressionStatement->expression;
    ASSERT(expression->type == LAMBDA_EXPRESSION);

    LambdaExpression* lambdaExpr = expression->as.lambdaExpression;
    ASSERT(lambdaExpr->parameters->used == 0);  // No parameters

    ASSERT(lambdaExpr->bodyBlock->statementArray.used == 1);
    ASSERT(lambdaExpr->bodyBlock->statementArray.values[0]->type == PRINT_STATEMENT);
    ASSERT(lambdaExpr->bodyBlock->statementArray.values[0]->as.printStatement->expression->type == PRIMARY_EXPRESSION);
    ASSERT(lambdaExpr->bodyBlock->statementArray.values[0]->as.printStatement->expression->as.primaryExpression->literal->type == STRING_LITERAL);

    ASSERT(lambdaExpr->bodyBlock->lastExpression == NULL);

    freeParserButNotAST(&parser);
    return SUCCESS_RETURN_CODE;
}

int test_parser_call_with_args() {
    // val greet = lambda (name) { print "Hello, " + name + "!"; };
    // greet("Alice");
    // greet("Bob");");
    Token tokensArray[] = {
        createToken(TOKEN_VAL, "val"),
        createToken(TOKEN_IDENTIFIER, "greet"),
        createToken(TOKEN_EQUAL, "="),
        createToken(TOKEN_LAMBDA, "lambda"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_IDENTIFIER, "name"),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_LEFT_CURLY, "{"),
        createToken(TOKEN_PRINT, "print"),
        createToken(TOKEN_STRING, "\"Hello, \""),
        createToken(TOKEN_PLUS, "+"),
        createToken(TOKEN_IDENTIFIER, "name"),
        createToken(TOKEN_PLUS, "+"),
        createToken(TOKEN_STRING, "\"!\""),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_RIGHT_CURLY, "}"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_IDENTIFIER, "greet"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_STRING, "\"Alice\""),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_IDENTIFIER, "greet"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_STRING, "\"Bob\""),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = sizeof(tokensArray) / sizeof(Token),
        .size = sizeof(tokensArray) / sizeof(Token)};

    PARSE_TEST_AST
    ASSERT(source->numberOfStatements == 3);

    Statement* valDecl = source->rootStatements[0];
    ASSERT(valDecl->type == VAL_DECLARATION_STATEMENT);
    ASSERT(strcmp(valDecl->as.valDeclarationStatement->identifier->token.start, "greet") == 0);

    Expression* lambdaExpr = valDecl->as.valDeclarationStatement->expression;
    ASSERT(lambdaExpr->type == LAMBDA_EXPRESSION);
    ASSERT(lambdaExpr->as.lambdaExpression->parameters->used == 1);

    Statement* callAlice = source->rootStatements[1];
    ASSERT(callAlice->type == EXPRESSION_STATEMENT);
    ASSERT(callAlice->as.expressionStatement->expression->type == CALL_EXPRESSION);
    ASSERT(strcmp(callAlice->as.expressionStatement->expression->as.callExpression->leftHandSide->as.primaryExpression->literal->as.identifierLiteral->token.start, "greet") == 0);
    ASSERT(callAlice->as.expressionStatement->expression->as.callExpression->arguments->used == 1);

    Statement* callBob = source->rootStatements[2];
    ASSERT(callBob->type == EXPRESSION_STATEMENT);
    ASSERT(callBob->as.expressionStatement->expression->type == CALL_EXPRESSION);
    ASSERT(strcmp(callBob->as.expressionStatement->expression->as.callExpression->leftHandSide->as.primaryExpression->literal->as.identifierLiteral->token.start, "greet") == 0);
    ASSERT(callBob->as.expressionStatement->expression->as.callExpression->arguments->used == 1);

    freeParserButNotAST(&parser);
    return SUCCESS_RETURN_CODE;
}

int test_parser_call_in_binary_expression() {
    // val double = lambda (n) { n * 2; };
    // val result = double(5) + 10;
    Token tokensArray[] = {
        createToken(TOKEN_VAL, "val"),
        createToken(TOKEN_IDENTIFIER, "double"),
        createToken(TOKEN_EQUAL, "="),
        createToken(TOKEN_LAMBDA, "lambda"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_IDENTIFIER, "n"),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_LEFT_CURLY, "{"),
        createToken(TOKEN_IDENTIFIER, "n"),
        createToken(TOKEN_STAR, "*"),
        createToken(TOKEN_NUMBER, "2"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_RIGHT_CURLY, "}"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_VAL, "val"),
        createToken(TOKEN_IDENTIFIER, "result"),
        createToken(TOKEN_EQUAL, "="),
        createToken(TOKEN_IDENTIFIER, "double"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_NUMBER, "5"),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_PLUS, "+"),
        createToken(TOKEN_NUMBER, "10"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = sizeof(tokensArray) / sizeof(Token),
        .size = sizeof(tokensArray) / sizeof(Token)};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 2);

    Statement* valResult = source->rootStatements[1];
    ASSERT(valResult->type == VAL_DECLARATION_STATEMENT);
    ASSERT(strcmp(valResult->as.valDeclarationStatement->identifier->token.start, "result") == 0);

    Expression* addExpr = valResult->as.valDeclarationStatement->expression;
    ASSERT(addExpr->type == ADDITIVE_EXPRESSION);
    ASSERT(addExpr->as.additiveExpression->leftExpression->type == CALL_EXPRESSION);
    ASSERT(strcmp(addExpr->as.additiveExpression->leftExpression->as.callExpression->leftHandSide->as.primaryExpression->literal->as.identifierLiteral->token.start, "double") == 0);
    ASSERT(addExpr->as.additiveExpression->leftExpression->as.callExpression->arguments->used == 1);
    ASSERT(addExpr->as.additiveExpression->rightExpression->type == PRIMARY_EXPRESSION);
    ASSERT(addExpr->as.additiveExpression->rightExpression->as.primaryExpression->literal->type == NUMBER_LITERAL);
    ASSERT(strcmp(addExpr->as.additiveExpression->rightExpression->as.primaryExpression->literal->as.numberLiteral->token.start, "10") == 0);

    freeParserButNotAST(&parser);
    return SUCCESS_RETURN_CODE;
}

int test_parser_call_no_args() {
    // val myFunc = lambda () { print "Called myFunc!"; }; myFunc();
    Token tokensArray[] = {
        createToken(TOKEN_VAL, "val"),
        createToken(TOKEN_IDENTIFIER, "myFunc"),
        createToken(TOKEN_EQUAL, "="),
        createToken(TOKEN_LAMBDA, "lambda"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_LEFT_CURLY, "{"),
        createToken(TOKEN_PRINT, "print"),
        createToken(TOKEN_STRING, "Called myFunc!"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_RIGHT_CURLY, "}"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_IDENTIFIER, "myFunc"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = sizeof(tokensArray) / sizeof(Token),
        .size = sizeof(tokensArray) / sizeof(Token)};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 2);

    Statement* callMyFunc = source->rootStatements[1];
    ASSERT(callMyFunc->type == EXPRESSION_STATEMENT);

    Expression* callExpr = callMyFunc->as.expressionStatement->expression;
    ASSERT(callExpr->type == CALL_EXPRESSION);
    ASSERT(strcmp(callExpr->as.callExpression->leftHandSide->as.primaryExpression->literal->as.identifierLiteral->token.start, "myFunc") == 0);
    ASSERT(callExpr->as.callExpression->arguments->used == 0);

    freeParserButNotAST(&parser);
    return SUCCESS_RETURN_CODE;
}

// TODO: the if-statement here should actually be the final expression somehow
int test_parser_recursive_call() {
    // val factorial = lambda (n) {
    //     if (n <= 1) { 1; } else { n * factorial(n - 1); }
    //  };
    Token tokensArray[] = {
        createToken(TOKEN_VAL, "val"),
        createToken(TOKEN_IDENTIFIER, "factorial"),
        createToken(TOKEN_EQUAL, "="),
        createToken(TOKEN_LAMBDA, "lambda"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_IDENTIFIER, "n"),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_LEFT_CURLY, "{"),
        createToken(TOKEN_IF, "if"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_IDENTIFIER, "n"),
        createToken(TOKEN_LESSER_EQUAL, "<="),
        createToken(TOKEN_NUMBER, "1"),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_LEFT_CURLY, "{"),
        createToken(TOKEN_NUMBER, "1"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_RIGHT_CURLY, "}"),
        createToken(TOKEN_ELSE, "else"),
        createToken(TOKEN_LEFT_CURLY, "{"),
        createToken(TOKEN_IDENTIFIER, "n"),
        createToken(TOKEN_STAR, "*"),
        createToken(TOKEN_IDENTIFIER, "factorial"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_IDENTIFIER, "n"),
        createToken(TOKEN_MINUS, "-"),
        createToken(TOKEN_NUMBER, "1"),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_RIGHT_CURLY, "}"),
        createToken(TOKEN_RIGHT_CURLY, "}"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = sizeof(tokensArray) / sizeof(Token),
        .size = sizeof(tokensArray) / sizeof(Token)};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 1);

    Statement* valFactorial = source->rootStatements[0];
    ASSERT(valFactorial->type == VAL_DECLARATION_STATEMENT);
    ASSERT(strcmp(valFactorial->as.valDeclarationStatement->identifier->token.start, "factorial") == 0);

    Expression* lambdaExpr = valFactorial->as.valDeclarationStatement->expression;
    ASSERT(lambdaExpr->type == LAMBDA_EXPRESSION);

    Statement* ifStmt = lambdaExpr->as.lambdaExpression->bodyBlock->statementArray.values[0];  // TODO: the if-statement here should actually be the final expression somehow
    ASSERT(ifStmt->type == SELECTION_STATEMENT);

    Expression* recursiveCallExpr = ifStmt->as.selectionStatement->falseStatement->as.blockStatement->statementArray.values[0]->as.expressionStatement->expression;
    ASSERT(recursiveCallExpr->type == MULTIPLICATIVE_EXPRESSION);
    ASSERT(recursiveCallExpr->as.multiplicativeExpression->rightExpression->type == CALL_EXPRESSION);
    ASSERT(strcmp(recursiveCallExpr->as.multiplicativeExpression->rightExpression->as.callExpression->leftHandSide->as.primaryExpression->literal->as.identifierLiteral->token.start, "factorial") == 0);
    ASSERT(recursiveCallExpr->as.multiplicativeExpression->rightExpression->as.callExpression->arguments->used == 1);

    freeParserButNotAST(&parser);

    return SUCCESS_RETURN_CODE;
}

int test_parser_call_with_block_expression_arg() {
    // val applyOperation = lambda (a, operation) { operation(a); };
    // val result = applyOperation(5, lambda(x) { x * x; });
    Token tokensArray[] = {
        createToken(TOKEN_VAL, "val"),
        createToken(TOKEN_IDENTIFIER, "applyOperation"),
        createToken(TOKEN_EQUAL, "="),
        createToken(TOKEN_LAMBDA, "lambda"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_IDENTIFIER, "a"),
        createToken(TOKEN_COMMA, ","),
        createToken(TOKEN_IDENTIFIER, "operation"),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_LEFT_CURLY, "{"),
        createToken(TOKEN_IDENTIFIER, "operation"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_IDENTIFIER, "a"),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_RIGHT_CURLY, "}"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_VAL, "val"),
        createToken(TOKEN_IDENTIFIER, "result"),
        createToken(TOKEN_EQUAL, "="),
        createToken(TOKEN_IDENTIFIER, "applyOperation"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_NUMBER, "5"),
        createToken(TOKEN_COMMA, ","),
        createToken(TOKEN_LAMBDA, "lambda"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_IDENTIFIER, "x"),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_LEFT_CURLY, "{"),
        createToken(TOKEN_IDENTIFIER, "x"),
        createToken(TOKEN_STAR, "*"),
        createToken(TOKEN_IDENTIFIER, "x"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_RIGHT_CURLY, "}"),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = sizeof(tokensArray) / sizeof(Token),
        .size = sizeof(tokensArray) / sizeof(Token)};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 2);

    Statement* valResult = source->rootStatements[1];
    ASSERT(valResult->type == VAL_DECLARATION_STATEMENT);
    ASSERT(strcmp(valResult->as.valDeclarationStatement->identifier->token.start, "result") == 0);

    Expression* callExpr = valResult->as.valDeclarationStatement->expression;
    ASSERT(callExpr->type == CALL_EXPRESSION);
    ASSERT(strcmp(callExpr->as.callExpression->leftHandSide->as.primaryExpression->literal->as.identifierLiteral->token.start, "applyOperation") == 0);
    ASSERT(callExpr->as.callExpression->arguments->used == 2);
    ASSERT(callExpr->as.callExpression->arguments->values[1]->type == LAMBDA_EXPRESSION);

    freeParserButNotAST(&parser);

    return SUCCESS_RETURN_CODE;
}

int test_parser_nested_calls() {
    // val addTwo = lambda (n) { n + 2; };
    // val multiplyByThree = lambda (n) { n * 3; };
    // val result = multiplyByThree(addTwo(5));
    Token tokensArray[] = {
        createToken(TOKEN_VAL, "val"),
        createToken(TOKEN_IDENTIFIER, "addTwo"),
        createToken(TOKEN_EQUAL, "="),
        createToken(TOKEN_LAMBDA, "lambda"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_IDENTIFIER, "n"),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_LEFT_CURLY, "{"),
        createToken(TOKEN_IDENTIFIER, "n"),
        createToken(TOKEN_PLUS, "+"),
        createToken(TOKEN_NUMBER, "2"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_RIGHT_CURLY, "}"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_VAL, "val"),
        createToken(TOKEN_IDENTIFIER, "multiplyByThree"),
        createToken(TOKEN_EQUAL, "="),
        createToken(TOKEN_LAMBDA, "lambda"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_IDENTIFIER, "n"),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_LEFT_CURLY, "{"),
        createToken(TOKEN_IDENTIFIER, "n"),
        createToken(TOKEN_STAR, "*"),
        createToken(TOKEN_NUMBER, "3"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_RIGHT_CURLY, "}"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_VAL, "val"),
        createToken(TOKEN_IDENTIFIER, "result"),
        createToken(TOKEN_EQUAL, "="),
        createToken(TOKEN_IDENTIFIER, "multiplyByThree"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_IDENTIFIER, "addTwo"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_NUMBER, "5"),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = sizeof(tokensArray) / sizeof(Token),
        .size = sizeof(tokensArray) / sizeof(Token)};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 3);

    Statement* valResult = source->rootStatements[2];
    ASSERT(valResult->type == VAL_DECLARATION_STATEMENT);
    ASSERT(strcmp(valResult->as.valDeclarationStatement->identifier->token.start, "result") == 0);

    Expression* callMultiplyByThree = valResult->as.valDeclarationStatement->expression;
    ASSERT(callMultiplyByThree->type == CALL_EXPRESSION);
    ASSERT(strcmp(callMultiplyByThree->as.callExpression->leftHandSide->as.primaryExpression->literal->as.identifierLiteral->token.start, "multiplyByThree") == 0);
    ASSERT(callMultiplyByThree->as.callExpression->arguments->used == 1);

    Expression* callAddTwo = callMultiplyByThree->as.callExpression->arguments->values[0];
    ASSERT(callAddTwo->type == CALL_EXPRESSION);
    ASSERT(strcmp(callAddTwo->as.callExpression->leftHandSide->as.primaryExpression->literal->as.identifierLiteral->token.start, "addTwo") == 0);
    ASSERT(callAddTwo->as.callExpression->arguments->used == 1);
    ASSERT(callAddTwo->as.callExpression->arguments->values[0]->type == PRIMARY_EXPRESSION);
    ASSERT(callAddTwo->as.callExpression->arguments->values[0]->as.primaryExpression->literal->type == NUMBER_LITERAL);
    ASSERT(strcmp(callAddTwo->as.callExpression->arguments->values[0]->as.primaryExpression->literal->as.numberLiteral->token.start, "5") == 0);

    freeParserButNotAST(&parser);

    return SUCCESS_RETURN_CODE;
}

int test_parser_call_with_expression_args() {
    // val sum = lambda (a, b) { a + b; };
    // val x = 10;
    // val y = 20;
    // val result = sum(x + 5, y - 3);
    Token tokensArray[] = {
        createToken(TOKEN_VAL, "val"),
        createToken(TOKEN_IDENTIFIER, "sum"),
        createToken(TOKEN_EQUAL, "="),
        createToken(TOKEN_LAMBDA, "lambda"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_IDENTIFIER, "a"),
        createToken(TOKEN_COMMA, ","),
        createToken(TOKEN_IDENTIFIER, "b"),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_LEFT_CURLY, "{"),
        createToken(TOKEN_IDENTIFIER, "a"),
        createToken(TOKEN_PLUS, "+"),
        createToken(TOKEN_IDENTIFIER, "b"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_RIGHT_CURLY, "}"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_VAL, "val"),
        createToken(TOKEN_IDENTIFIER, "x"),
        createToken(TOKEN_EQUAL, "="),
        createToken(TOKEN_NUMBER, "10"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_VAL, "val"),
        createToken(TOKEN_IDENTIFIER, "y"),
        createToken(TOKEN_EQUAL, "="),
        createToken(TOKEN_NUMBER, "20"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_VAL, "val"),
        createToken(TOKEN_IDENTIFIER, "result"),
        createToken(TOKEN_EQUAL, "="),
        createToken(TOKEN_IDENTIFIER, "sum"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_IDENTIFIER, "x"),
        createToken(TOKEN_PLUS, "+"),
        createToken(TOKEN_NUMBER, "5"),
        createToken(TOKEN_COMMA, ","),
        createToken(TOKEN_IDENTIFIER, "y"),
        createToken(TOKEN_MINUS, "-"),
        createToken(TOKEN_NUMBER, "3"),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = sizeof(tokensArray) / sizeof(Token),
        .size = sizeof(tokensArray) / sizeof(Token)};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 4);

    Statement* valResult = source->rootStatements[3];
    ASSERT(valResult->type == VAL_DECLARATION_STATEMENT);
    ASSERT(strcmp(valResult->as.valDeclarationStatement->identifier->token.start, "result") == 0);

    Expression* callExpr = valResult->as.valDeclarationStatement->expression;
    ASSERT(callExpr->type == CALL_EXPRESSION);
    ASSERT(strcmp(callExpr->as.callExpression->leftHandSide->as.primaryExpression->literal->as.identifierLiteral->token.start, "sum") == 0);
    ASSERT(callExpr->as.callExpression->arguments->used == 2);

    Expression* firstArg = callExpr->as.callExpression->arguments->values[0];
    ASSERT(firstArg->type == ADDITIVE_EXPRESSION);
    ASSERT(firstArg->as.additiveExpression->leftExpression->type == PRIMARY_EXPRESSION);
    ASSERT(strcmp(firstArg->as.additiveExpression->leftExpression->as.primaryExpression->literal->as.identifierLiteral->token.start, "x") == 0);
    ASSERT(firstArg->as.additiveExpression->rightExpression->type == PRIMARY_EXPRESSION);
    ASSERT(strcmp(firstArg->as.additiveExpression->rightExpression->as.primaryExpression->literal->as.numberLiteral->token.start, "5") == 0);

    Expression* secondArg = callExpr->as.callExpression->arguments->values[1];
    ASSERT(secondArg->type == ADDITIVE_EXPRESSION);
    ASSERT(secondArg->as.additiveExpression->leftExpression->type == PRIMARY_EXPRESSION);
    ASSERT(strcmp(secondArg->as.additiveExpression->leftExpression->as.primaryExpression->literal->as.identifierLiteral->token.start, "y") == 0);
    ASSERT(secondArg->as.additiveExpression->rightExpression->type == PRIMARY_EXPRESSION);
    ASSERT(strcmp(secondArg->as.additiveExpression->rightExpression->as.primaryExpression->literal->as.numberLiteral->token.start, "3") == 0);

    freeParserButNotAST(&parser);

    return SUCCESS_RETURN_CODE;
}

int test_parser_call_in_if_condition() {
    // val isTen = lambda (n) { n == 10; };
    // val num = 10;
    // if (isTen(num)) { print \"10 is 10.\"; } else { print \"10 is not 10.\"; }
    Token tokensArray[] = {
        createToken(TOKEN_VAL, "val"),
        createToken(TOKEN_IDENTIFIER, "isTen"),
        createToken(TOKEN_EQUAL, "="),
        createToken(TOKEN_LAMBDA, "lambda"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_IDENTIFIER, "n"),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_LEFT_CURLY, "{"),
        createToken(TOKEN_IDENTIFIER, "n"),
        createToken(TOKEN_EQUAL_EQUAL, "=="),
        createToken(TOKEN_NUMBER, "10"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_RIGHT_CURLY, "}"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_VAL, "val"),
        createToken(TOKEN_IDENTIFIER, "num"),
        createToken(TOKEN_EQUAL, "="),
        createToken(TOKEN_NUMBER, "10"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_IF, "if"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_IDENTIFIER, "isTen"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_IDENTIFIER, "num"),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_LEFT_CURLY, "{"),
        createToken(TOKEN_PRINT, "print"),
        createToken(TOKEN_STRING, "\"10 is 10.\""),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_RIGHT_CURLY, "}"),
        createToken(TOKEN_ELSE, "else"),
        createToken(TOKEN_LEFT_CURLY, "{"),
        createToken(TOKEN_PRINT, "print"),
        createToken(TOKEN_STRING, "\"10 is not 10.\""),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_RIGHT_CURLY, "}"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = sizeof(tokensArray) / sizeof(Token),
        .size = sizeof(tokensArray) / sizeof(Token)};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 3);

    Statement* ifStmt = source->rootStatements[2];
    ASSERT(ifStmt->type == SELECTION_STATEMENT);

    Expression* conditionExpr = ifStmt->as.selectionStatement->conditionExpression;
    ASSERT(conditionExpr->type == CALL_EXPRESSION);
    ASSERT(strcmp(conditionExpr->as.callExpression->leftHandSide->as.primaryExpression->literal->as.identifierLiteral->token.start, "isTen") == 0);
    ASSERT(conditionExpr->as.callExpression->arguments->used == 1);
    ASSERT(conditionExpr->as.callExpression->arguments->values[0]->type == PRIMARY_EXPRESSION);
    ASSERT(strcmp(conditionExpr->as.callExpression->arguments->values[0]->as.primaryExpression->literal->as.identifierLiteral->token.start, "num") == 0);

    freeParserButNotAST(&parser);

    return SUCCESS_RETURN_CODE;
}

int test_parser_chained_calls() {
    // add(1, 2)(3, 4)(5, 6)
    Token tokensArray[] = {
        createToken(TOKEN_IDENTIFIER, "add"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_NUMBER, "1"),
        createToken(TOKEN_COMMA, ","),
        createToken(TOKEN_NUMBER, "2"),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_NUMBER, "3"),
        createToken(TOKEN_COMMA, ","),
        createToken(TOKEN_NUMBER, "4"),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_NUMBER, "5"),
        createToken(TOKEN_COMMA, ","),
        createToken(TOKEN_NUMBER, "6"),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_SEMICOLON, ")"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = sizeof(tokensArray) / sizeof(Token),
        .size = sizeof(tokensArray) / sizeof(Token)};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 1);
    Statement* stmt = source->rootStatements[0];
    ASSERT(stmt->type == EXPRESSION_STATEMENT);

    Expression* expr = stmt->as.expressionStatement->expression;
    ASSERT(expr->type == CALL_EXPRESSION);

    CallExpression* call1 = expr->as.callExpression;
    ASSERT(call1->arguments->used == 2);
    ASSERT(call1->leftHandSide->type == CALL_EXPRESSION);

    CallExpression* call2 = call1->leftHandSide->as.callExpression;
    ASSERT(call2->arguments->used == 2);
    ASSERT(call2->leftHandSide->type == CALL_EXPRESSION);

    CallExpression* call3 = call2->leftHandSide->as.callExpression;
    ASSERT(call3->arguments->used == 2);
    ASSERT(call3->leftHandSide->type == PRIMARY_EXPRESSION);

    PrimaryExpression* primary = call3->leftHandSide->as.primaryExpression;
    ASSERT(primary->literal->type == IDENTIFIER_LITERAL);
    ASSERT(strcmp(primary->literal->as.identifierLiteral->token.start, "add") == 0);

    freeParserButNotAST(&parser);

    return SUCCESS_RETURN_CODE;
}

int test_parser_simple_return() {
    // return 42;
    Token tokensArray[] = {
        createToken(TOKEN_RETURN, "return"),
        createToken(TOKEN_NUMBER, "42"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = 4,
        .size = 4};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 1);
    Statement* statement = source->rootStatements[0];
    ASSERT(statement->type == RETURN_STATEMENT);
    ReturnStatement* returnStmt = statement->as.returnStatement;
    ASSERT(returnStmt->expression->type == PRIMARY_EXPRESSION);
    ASSERT(returnStmt->expression->as.primaryExpression->literal->type == NUMBER_LITERAL);
    ASSERT(strcmp(returnStmt->expression->as.primaryExpression->literal->as.numberLiteral->token.start, "42") == 0);

    freeParserButNotAST(&parser);
    return SUCCESS_RETURN_CODE;
}

int test_parser_return_without_expression() {
    // return;
    Token tokensArray[] = {
        createToken(TOKEN_RETURN, "return"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = 3,
        .size = 3};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 1);
    Statement* statement = source->rootStatements[0];
    ASSERT(statement->type == RETURN_STATEMENT);
    ReturnStatement* returnStmt = statement->as.returnStatement;
    ASSERT(returnStmt->expression == NULL);

    freeParserButNotAST(&parser);
    return SUCCESS_RETURN_CODE;
}

int test_parser_return_in_lambda() {
    // val func = lambda () { return 10; };
    Token tokensArray[] = {
        createToken(TOKEN_VAL, "val"),
        createToken(TOKEN_IDENTIFIER, "func"),
        createToken(TOKEN_EQUAL, "="),
        createToken(TOKEN_LAMBDA, "lambda"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_LEFT_CURLY, "{"),
        createToken(TOKEN_RETURN, "return"),
        createToken(TOKEN_NUMBER, "10"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_RIGHT_CURLY, "}"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = 13,
        .size = 13};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 1);
    Statement* statement = source->rootStatements[0];
    ASSERT(statement->type == VAL_DECLARATION_STATEMENT);
    ValDeclarationStatement* valDecl = statement->as.valDeclarationStatement;
    ASSERT(valDecl->expression->type == LAMBDA_EXPRESSION);

    LambdaExpression* lambda = valDecl->expression->as.lambdaExpression;
    ASSERT(lambda->bodyBlock->statementArray.used == 1);
    Statement* lambdaStatement = lambda->bodyBlock->statementArray.values[0];
    ASSERT(lambdaStatement->type == RETURN_STATEMENT);
    ReturnStatement* returnStmt = lambdaStatement->as.returnStatement;
    ASSERT(returnStmt->expression->type == PRIMARY_EXPRESSION);
    ASSERT(returnStmt->expression->as.primaryExpression->literal->type == NUMBER_LITERAL);
    ASSERT(strcmp(returnStmt->expression->as.primaryExpression->literal->as.numberLiteral->token.start, "10") == 0);

    freeParserButNotAST(&parser);
    return SUCCESS_RETURN_CODE;
}

int test_parser_multiple_returns() {
    // val func = lambda (x) { if (x > 0) { return 1; } else { return -1; } };
    Token tokensArray[] = {
        createToken(TOKEN_VAL, "val"),
        createToken(TOKEN_IDENTIFIER, "func"),
        createToken(TOKEN_EQUAL, "="),
        createToken(TOKEN_LAMBDA, "lambda"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_IDENTIFIER, "x"),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_LEFT_CURLY, "{"),
        createToken(TOKEN_IF, "if"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_IDENTIFIER, "x"),
        createToken(TOKEN_GREATER, ">"),
        createToken(TOKEN_NUMBER, "0"),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_LEFT_CURLY, "{"),
        createToken(TOKEN_RETURN, "return"),
        createToken(TOKEN_NUMBER, "1"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_RIGHT_CURLY, "}"),
        createToken(TOKEN_ELSE, "else"),
        createToken(TOKEN_LEFT_CURLY, "{"),
        createToken(TOKEN_RETURN, "return"),
        createToken(TOKEN_MINUS, "-"),
        createToken(TOKEN_NUMBER, "1"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_RIGHT_CURLY, "}"),
        createToken(TOKEN_RIGHT_CURLY, "}"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = 29,
        .size = 29};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 1);
    Statement* statement = source->rootStatements[0];
    ASSERT(statement->type == VAL_DECLARATION_STATEMENT);
    ValDeclarationStatement* valDecl = statement->as.valDeclarationStatement;
    ASSERT(valDecl->expression->type == LAMBDA_EXPRESSION);

    LambdaExpression* lambda = valDecl->expression->as.lambdaExpression;
    ASSERT(lambda->bodyBlock->statementArray.used == 1);
    Statement* ifStatement = lambda->bodyBlock->statementArray.values[0];
    ASSERT(ifStatement->type == SELECTION_STATEMENT);

    SelectionStatement* selection = ifStatement->as.selectionStatement;
    ASSERT(selection->trueStatement->type == BLOCK_STATEMENT);
    ASSERT(selection->falseStatement->type == BLOCK_STATEMENT);

    BlockStatement* trueBlock = selection->trueStatement->as.blockStatement;
    ASSERT(trueBlock->statementArray.used == 1);
    ASSERT(trueBlock->statementArray.values[0]->type == RETURN_STATEMENT);

    BlockStatement* falseBlock = selection->falseStatement->as.blockStatement;
    ASSERT(falseBlock->statementArray.used == 1);
    ASSERT(falseBlock->statementArray.values[0]->type == RETURN_STATEMENT);

    freeParserButNotAST(&parser);
    return SUCCESS_RETURN_CODE;
}

int test_parser_member_expression_simple() {
    // a.b;
    Token tokensArray[] = {
        createToken(TOKEN_IDENTIFIER, "a"),
        createToken(TOKEN_DOT, "."),
        createToken(TOKEN_IDENTIFIER, "b"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = sizeof(tokensArray) / sizeof(Token),
        .size = sizeof(tokensArray) / sizeof(Token)};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 1);
    Statement* stmt = source->rootStatements[0];
    ASSERT(stmt->type == EXPRESSION_STATEMENT);
    Expression* expr = stmt->as.expressionStatement->expression;
    ASSERT(expr->type == MEMBER_EXPRESSION);
    ASSERT(expr->as.memberExpression->leftHandSide->type == PRIMARY_EXPRESSION);
    ASSERT(expr->as.memberExpression->leftHandSide->as.primaryExpression->literal->type == IDENTIFIER_LITERAL);
    ASSERT(strcmp(expr->as.memberExpression->leftHandSide->as.primaryExpression->literal->as.identifierLiteral->token.start, "a") == 0);
    ASSERT(expr->as.memberExpression->rightHandSide->type == PRIMARY_EXPRESSION);
    ASSERT(expr->as.memberExpression->rightHandSide->as.primaryExpression->literal->type == IDENTIFIER_LITERAL);
    ASSERT(strcmp(expr->as.memberExpression->rightHandSide->as.primaryExpression->literal->as.identifierLiteral->token.start, "b") == 0);

    freeParserButNotAST(&parser);
    return SUCCESS_RETURN_CODE;
}

int test_parser_member_and_call_expression() {
    // a.b();
    Token tokensArray[] = {
        createToken(TOKEN_IDENTIFIER, "a"),
        createToken(TOKEN_DOT, "."),
        createToken(TOKEN_IDENTIFIER, "b"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = sizeof(tokensArray) / sizeof(Token),
        .size = sizeof(tokensArray) / sizeof(Token)};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 1);
    Statement* stmt = source->rootStatements[0];
    ASSERT(stmt->type == EXPRESSION_STATEMENT);
    Expression* expr = stmt->as.expressionStatement->expression;
    ASSERT(expr->type == CALL_EXPRESSION);
    ASSERT(expr->as.callExpression->leftHandSide->type == MEMBER_EXPRESSION);
    ASSERT(expr->as.callExpression->leftHandSide->as.memberExpression->leftHandSide->type == PRIMARY_EXPRESSION);
    ASSERT(expr->as.callExpression->leftHandSide->as.memberExpression->leftHandSide->as.primaryExpression->literal->type == IDENTIFIER_LITERAL);
    ASSERT(strcmp(expr->as.callExpression->leftHandSide->as.memberExpression->leftHandSide->as.primaryExpression->literal->as.identifierLiteral->token.start, "a") == 0);
    ASSERT(expr->as.callExpression->leftHandSide->as.memberExpression->rightHandSide->type == PRIMARY_EXPRESSION);
    ASSERT(expr->as.callExpression->leftHandSide->as.memberExpression->rightHandSide->as.primaryExpression->literal->type == IDENTIFIER_LITERAL);
    ASSERT(strcmp(expr->as.callExpression->leftHandSide->as.memberExpression->rightHandSide->as.primaryExpression->literal->as.identifierLiteral->token.start, "b") == 0);
    ASSERT(expr->as.callExpression->arguments->used == 0);

    freeParserButNotAST(&parser);
    return SUCCESS_RETURN_CODE;
}

int test_parser_complex_member_and_call_expression() {
    // a().b().c;
    Token tokensArray[] = {
        createToken(TOKEN_IDENTIFIER, "a"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_DOT, "."),
        createToken(TOKEN_IDENTIFIER, "b"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_DOT, "."),
        createToken(TOKEN_IDENTIFIER, "c"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = sizeof(tokensArray) / sizeof(Token),
        .size = sizeof(tokensArray) / sizeof(Token)};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 1);
    Statement* stmt = source->rootStatements[0];
    ASSERT(stmt->type == EXPRESSION_STATEMENT);
    Expression* expr = stmt->as.expressionStatement->expression;
    ASSERT(expr->type == MEMBER_EXPRESSION);
    ASSERT(expr->as.memberExpression->leftHandSide->type == CALL_EXPRESSION);
    ASSERT(expr->as.memberExpression->leftHandSide->as.callExpression->leftHandSide->type == MEMBER_EXPRESSION);
    ASSERT(expr->as.memberExpression->leftHandSide->as.callExpression->leftHandSide->as.memberExpression->leftHandSide->type == CALL_EXPRESSION);
    ASSERT(expr->as.memberExpression->leftHandSide->as.callExpression->leftHandSide->as.memberExpression->leftHandSide->as.callExpression->leftHandSide->type == PRIMARY_EXPRESSION);
    ASSERT(expr->as.memberExpression->leftHandSide->as.callExpression->leftHandSide->as.memberExpression->leftHandSide->as.callExpression->leftHandSide->as.primaryExpression->literal->type == IDENTIFIER_LITERAL);
    ASSERT(strcmp(expr->as.memberExpression->leftHandSide->as.callExpression->leftHandSide->as.memberExpression->leftHandSide->as.callExpression->leftHandSide->as.primaryExpression->literal->as.identifierLiteral->token.start, "a") == 0);
    ASSERT(expr->as.memberExpression->rightHandSide->type == PRIMARY_EXPRESSION);
    ASSERT(expr->as.memberExpression->rightHandSide->as.primaryExpression->literal->type == IDENTIFIER_LITERAL);
    ASSERT(strcmp(expr->as.memberExpression->rightHandSide->as.primaryExpression->literal->as.identifierLiteral->token.start, "c") == 0);

    freeParserButNotAST(&parser);
    return SUCCESS_RETURN_CODE;
}

int test_parser_nested_call_expression() {
    // a()()();
    Token tokensArray[] = {
        createToken(TOKEN_IDENTIFIER, "a"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_LEFT_PAREN, "("),
        createToken(TOKEN_RIGHT_PAREN, ")"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = sizeof(tokensArray) / sizeof(Token),
        .size = sizeof(tokensArray) / sizeof(Token)};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 1);
    Statement* stmt = source->rootStatements[0];
    ASSERT(stmt->type == EXPRESSION_STATEMENT);
    Expression* expr = stmt->as.expressionStatement->expression;
    ASSERT(expr->type == CALL_EXPRESSION);
    ASSERT(expr->as.callExpression->leftHandSide->type == CALL_EXPRESSION);
    ASSERT(expr->as.callExpression->leftHandSide->as.callExpression->leftHandSide->type == CALL_EXPRESSION);
    ASSERT(expr->as.callExpression->leftHandSide->as.callExpression->leftHandSide->as.callExpression->leftHandSide->type == PRIMARY_EXPRESSION);
    ASSERT(expr->as.callExpression->leftHandSide->as.callExpression->leftHandSide->as.callExpression->leftHandSide->as.primaryExpression->literal->type == IDENTIFIER_LITERAL);
    ASSERT(strcmp(expr->as.callExpression->leftHandSide->as.callExpression->leftHandSide->as.callExpression->leftHandSide->as.primaryExpression->literal->as.identifierLiteral->token.start, "a") == 0);

    freeParserButNotAST(&parser);
    return SUCCESS_RETURN_CODE;
}

int test_parser_empty_struct() {
    Token tokensArray[] = {
        createToken(TOKEN_STRUCT, "struct"),
        createToken(TOKEN_LEFT_CURLY, "{"),
        createToken(TOKEN_RIGHT_CURLY, "}"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = sizeof(tokensArray) / sizeof(Token),
        .size = sizeof(tokensArray) / sizeof(Token)};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 1);
    Statement* stmt = source->rootStatements[0];
    ASSERT(stmt->type == EXPRESSION_STATEMENT);
    ASSERT(stmt->as.expressionStatement->expression->type == STRUCT_EXPRESSION);
    ASSERT(stmt->as.expressionStatement->expression->as.structExpression->declarationArray.used == 0);

    freeParserButNotAST(&parser);
    return SUCCESS_RETURN_CODE;
}

int test_parser_struct_with_single_declaration() {
    Token tokensArray[] = {
        createToken(TOKEN_STRUCT, "struct"),
        createToken(TOKEN_LEFT_CURLY, "{"),
        createToken(TOKEN_IDENTIFIER, "x"),
        createToken(TOKEN_COLON, ":"),
        createToken(TOKEN_NUMBER, "42"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_RIGHT_CURLY, "}"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = sizeof(tokensArray) / sizeof(Token),
        .size = sizeof(tokensArray) / sizeof(Token)};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 1);
    Statement* stmt = source->rootStatements[0];
    ASSERT(stmt->type == EXPRESSION_STATEMENT);
    ASSERT(stmt->as.expressionStatement->expression->type == STRUCT_EXPRESSION);

    StructExpression* structExpr = stmt->as.expressionStatement->expression->as.structExpression;
    ASSERT(structExpr->declarationArray.used == 1);
    ASSERT(strcmp(structExpr->declarationArray.values[0]->identifier->token.start, "x") == 0);
    ASSERT(structExpr->declarationArray.values[0]->maybeExpression->type == PRIMARY_EXPRESSION);
    ASSERT(structExpr->declarationArray.values[0]->maybeExpression->as.primaryExpression->literal->type == NUMBER_LITERAL);

    freeParserButNotAST(&parser);
    return SUCCESS_RETURN_CODE;
}

int test_parser_struct_with_multiple_declarations() {
    Token tokensArray[] = {
        createToken(TOKEN_STRUCT, "struct"),
        createToken(TOKEN_LEFT_CURLY, "{"),
        createToken(TOKEN_IDENTIFIER, "x"),
        createToken(TOKEN_COLON, ":"),
        createToken(TOKEN_NUMBER, "42"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_IDENTIFIER, "y"),
        createToken(TOKEN_COLON, ":"),
        createToken(TOKEN_STRING, "\"hello\""),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_PROTOTYPE, "prototype"),
        createToken(TOKEN_COLON, ":"),
        createToken(TOKEN_IDENTIFIER, "BaseStruct"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_RIGHT_CURLY, "}"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = sizeof(tokensArray) / sizeof(Token),
        .size = sizeof(tokensArray) / sizeof(Token)};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 1);
    Statement* stmt = source->rootStatements[0];
    ASSERT(stmt->type == EXPRESSION_STATEMENT);
    ASSERT(stmt->as.expressionStatement->expression->type == STRUCT_EXPRESSION);

    StructExpression* structExpr = stmt->as.expressionStatement->expression->as.structExpression;
    ASSERT(structExpr->declarationArray.used == 3);

    // Check first declaration
    ASSERT(strcmp(structExpr->declarationArray.values[0]->identifier->token.start, "x") == 0);
    ASSERT(structExpr->declarationArray.values[0]->maybeExpression->type == PRIMARY_EXPRESSION);
    ASSERT(structExpr->declarationArray.values[0]->maybeExpression->as.primaryExpression->literal->type == NUMBER_LITERAL);

    // Check second declaration
    ASSERT(strcmp(structExpr->declarationArray.values[1]->identifier->token.start, "y") == 0);
    ASSERT(structExpr->declarationArray.values[1]->maybeExpression->type == PRIMARY_EXPRESSION);
    ASSERT(structExpr->declarationArray.values[1]->maybeExpression->as.primaryExpression->literal->type == STRING_LITERAL);

    // Check prototype declaration
    ASSERT(structExpr->declarationArray.values[2]->isPrototype);
    ASSERT(strcmp(structExpr->declarationArray.values[2]->identifier->token.start, "BaseStruct") == 0);
    ASSERT(structExpr->declarationArray.values[2]->maybeExpression == NULL);

    freeParserButNotAST(&parser);
    return SUCCESS_RETURN_CODE;
}

// Add these additional test functions to parser_test.c

int test_parser_struct_with_nested_struct() {
    Token tokensArray[] = {
        createToken(TOKEN_STRUCT, "struct"),
        createToken(TOKEN_LEFT_CURLY, "{"),
        createToken(TOKEN_IDENTIFIER, "outer"),
        createToken(TOKEN_COLON, ":"),
        createToken(TOKEN_STRUCT, "struct"),
        createToken(TOKEN_LEFT_CURLY, "{"),
        createToken(TOKEN_IDENTIFIER, "inner"),
        createToken(TOKEN_COLON, ":"),
        createToken(TOKEN_NUMBER, "42"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_RIGHT_CURLY, "}"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_RIGHT_CURLY, "}"),
        createToken(TOKEN_SEMICOLON, ";"),
        createToken(TOKEN_EOF, "")};

    TokenArray tokens = {
        .values = tokensArray,
        .used = sizeof(tokensArray) / sizeof(Token),
        .size = sizeof(tokensArray) / sizeof(Token)};

    PARSE_TEST_AST

    ASSERT(source->numberOfStatements == 1);
    Statement* stmt = source->rootStatements[0];
    ASSERT(stmt->type == EXPRESSION_STATEMENT);
    ASSERT(stmt->as.expressionStatement->expression->type == STRUCT_EXPRESSION);

    StructExpression* outerStruct = stmt->as.expressionStatement->expression->as.structExpression;
    ASSERT(outerStruct->declarationArray.used == 1);
    ASSERT(strcmp(outerStruct->declarationArray.values[0]->identifier->token.start, "outer") == 0);
    ASSERT(outerStruct->declarationArray.values[0]->maybeExpression->type == STRUCT_EXPRESSION);

    StructExpression* innerStruct = outerStruct->declarationArray.values[0]->maybeExpression->as.structExpression;
    ASSERT(innerStruct->declarationArray.used == 1);
    ASSERT(strcmp(innerStruct->declarationArray.values[0]->identifier->token.start, "inner") == 0);
    ASSERT(innerStruct->declarationArray.values[0]->maybeExpression->type == PRIMARY_EXPRESSION);
    ASSERT(innerStruct->declarationArray.values[0]->maybeExpression->as.primaryExpression->literal->type == NUMBER_LITERAL);

    freeParserButNotAST(&parser);
    return SUCCESS_RETURN_CODE;
}