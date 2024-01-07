#include "debug.h"

#include <stdio.h>

#include "array.h"
#include "token.h"
#include "util/colors.h"

// Forward declarations
static void printStatement(const Statement* statement, int depth);
static void printExpression(const Expression* expression, int depth);
static void printLiteral(const Literal* literal, int depth);

// ---------------------------------------------------------------------------
// --------------------------------- SCANNER ---------------------------------
// ---------------------------------------------------------------------------

// Get the string name for a token. The order of the `TokenType` enum is assumed to be the same as this array's.
static char const* tokenTypeStrings[] = {
    // Keywords
    "TOKEN_IF",
    "TOKEN_ELSE",
    "TOKEN_STRUCT",
    "TOKEN_RETURN",
    "TOKEN_FALSE",
    "TOKEN_TRUE",
    "TOKEN_NULL",
    "TOKEN_VAL",

    // Identifier
    "TOKEN_IDENTIFIER",

    // String literal
    "TOKEN_STRING",

    // Number literal
    "TOKEN_NUMBER",

    // Punctuation
    "TOKEN_LEFT_PAREN",
    "TOKEN_RIGHT_PAREN",
    "TOKEN_LEFT_CURLY",
    "TOKEN_RIGHT_CURLY",
    "TOKEN_DOT",
    "TOKEN_STAR",
    "TOKEN_SLASH",
    "TOKEN_PLUS",
    "TOKEN_MINUS",
    "TOKEN_EXCLAMATION",
    "TOKEN_EXCLAMATION_EQUAL",
    "TOKEN_MODULO",
    "TOKEN_LESSER",
    "TOKEN_LESSER_EQUAL",
    "TOKEN_GREATER",
    "TOKEN_GREATER_EQUAL",
    "TOKEN_EQUAL",
    "TOKEN_EQUAL_EQUAL",
    "TOKEN_OR_OR",
    "TOKEN_AND_AND",
    "TOKEN_SEMICOLON",
    "TOKEN_COMMA",

    // Special tokens
    "TOKEN_ERROR",
    "TOKEN_EOF"};

char const* tokenTypeToString(TokenType tokenType) {
    return tokenTypeStrings[tokenType];
}

void printToken(Token token) {
    printf(" %s", tokenTypeToString(token.type));
    printf(KGRY "(lexeme=\"%.*s\", line=%d, column=%d)\n" RESET,
           token.length, token.start,
           token.lineNo,
           token.colNo);
}

void printTokenList(TokenArray tokenArray) {
    printf("Tokens:\n");

    for (int i = 0; i < tokenArray.used; i++) {
        printf(" ");
        printToken(tokenArray.values[i]);
    }
    printf("\n");
}

// ---------------------------------------------------------------------------
// --------------------------------- PARSER ----------------------------------
// ---------------------------------------------------------------------------

// Print indents for the given depth. This enables printing the AST in a tree-like format.
static void printIndent(int depth) {
    for (int i = 0; i < depth; ++i) {
        printf(KGRY "|   " RESET);
    }
}

void printAST(const Source* source) {
    if (source == NULL) {
        printf("<null AST>\n");
        return;
    }

    printf("AST:\n", source->numberOfStatements);
    printf("Source" KGRY "(numberOfStatements=%d)\n" RESET, source->numberOfStatements);
    for (int i = 0; i < source->numberOfStatements; ++i) {
        printIndent(0);
        printStatement(source->rootStatements[i], 1);
    }
    printf("\n");
}

static void printStatement(const Statement* statement, int depth) {
    if (statement == NULL) {
        printIndent(depth);
        printf("<null statement>\n");
        return;
    }

    printIndent(depth);

    switch (statement->type) {
        case EXPRESSION_STATEMENT:
            printf("ExpressionStatement\n");
            printExpression(statement->as.expressionStatement->expression, depth + 1);
            break;

        case VAL_DECLARATION_STATEMENT: {
            ValDeclarationStatement* valDecl = statement->as.valDeclarationStatement;
            printf("ValDeclaration" KGRY "(identifier=\"%.*s\")\n" RESET, valDecl->identifier->token.length, valDecl->identifier->token.start);
            printExpression(valDecl->expression, depth + 1);
            break;
        }
    }
}

static void printExpression(const Expression* expression, int depth) {
    if (expression == NULL) {
        printIndent(depth);
        printf("<null expression>\n");
        return;
    }

    printIndent(depth);

    switch (expression->type) {
        case PRIMARY_EXPRESSION:
            printf("PrimaryExpression\n");
            printLiteral(expression->as.primaryExpression->literal, depth + 1);
            break;

        case ADDITIVE_EXPRESSION: {
            AdditiveExpression* addExpr = expression->as.additiveExpression;
            // Assuming the token.start is a null-terminated string
            printf("AdditiveExpression" KGRY "(punctuator=\"%.*s\")\n" RESET, addExpr->punctuator->length, addExpr->punctuator->start);
            printIndent(depth + 1);
            printf(KGRY "(left)\n" RESET);
            printExpression(addExpr->leftExpression, depth + 2);
            printIndent(depth + 1);
            printf(KGRY "(right)\n" RESET);
            printExpression(addExpr->rightExpression, depth + 2);
            break;
        }
    }
}

static void printLiteral(const Literal* literal, int depth) {
    if (literal == NULL) {
        printIndent(depth);
        printf("<null literal>\n");
        return;
    }

    printIndent(depth);

    switch (literal->type) {
        case NUMBER_LITERAL:
            printf("NumberLiteral" KGRY "(token=\"%s\")\n" RESET, literal->as.numberLiteral->token.start);
            break;

        case IDENTIFIER_LITERAL:
            printf("IdentifierLiteral" KGRY "(token=\"%s\")\n" RESET, literal->as.identifierLiteral->token.start);
            break;

        case STRING_LITERAL:
            printf("StringLiteral" KGRY "(token=\"%s\")\n" RESET, literal->as.stringLiteral->token.start);
            break;

            // Add cases for other literal types
    }
}

// ---------------------------------------------------------------------------
// -------------------------------- COMPILER ---------------------------------
// ---------------------------------------------------------------------------

void printBytecodeArray(BytecodeArray bytecodeArray) {
    printf("Compiled bytecode:\n");

    for (int i = 0; i < bytecodeArray.used; i++) {
        switch (bytecodeArray.values[i].type) {
            case OP_ADD:
                printf(" [ ADD ]\n");
            case OP_CONSTANT:
                printf(" [ CONSTANT %f ]\n", bytecodeArray.values[i].operands.constant.as.doubleVal);
            case OP_PRINT:
                printf(" [ PRINT ]\n");
        }
    }
    printf("\n");
}
