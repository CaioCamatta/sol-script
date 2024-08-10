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
    "TOKEN_VAR",
    "TOKEN_PRINT",
    "TOKEN_WHILE",
    "TOKEN_LAMBDA",

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
    printf(KBOLD KCYN "Tokens\n" RESET KBOFF);

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

    printf(KBOLD KCYN "AST\n" RESET KBOFF);
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
        case VAR_DECLARATION_STATEMENT: {
            VarDeclarationStatement* varDecl = statement->as.varDeclarationStatement;
            printf("VarDeclaration" KGRY "(identifier=\"%.*s\")\n" RESET, varDecl->identifier->token.length, varDecl->identifier->token.start);
            if (varDecl->maybeExpression != NULL) printExpression(varDecl->maybeExpression, depth + 1);
            break;
        }
        case SELECTION_STATEMENT: {
            SelectionStatement* stmt = statement->as.selectionStatement;
            printf("SelectionStatement\n");
            printExpression(stmt->conditionExpression, depth + 1);
            printStatement(stmt->trueStatement, depth + 1);
            printStatement(stmt->falseStatement, depth + 1);
            break;
        }
        case ITERATION_STATEMENT: {
            IterationStatement* stmt = statement->as.iterationStatement;
            printf("IterationStatement\n");
            printExpression(stmt->conditionExpression, depth + 1);
            printStatement(stmt->bodyStatement, depth + 1);
            break;
        }
        case ASSIGNMENT_STATEMENT: {
            AssignmentStatement* stmt = statement->as.assignmentStatement;
            printf("AssignmentStatement\n");
            printExpression(stmt->target, depth + 1);
            printExpression(stmt->value, depth + 1);
            break;
        }
        case PRINT_STATEMENT:
            printf("PrintStatement\n");
            printExpression(statement->as.printStatement->expression, depth + 1);
            break;
        case RETURN_STATEMENT:
            printf("ReturnStatement\n");
            printExpression(statement->as.returnStatement->expression, depth + 1);
            break;
        case BLOCK_STATEMENT:
            printf("BlockStatement" KGRY "(numberOfStatements=%zu)\n" RESET, statement->as.blockStatement->statementArray.used);
            for (int i = 0; i < statement->as.blockStatement->statementArray.used; ++i) {
                printStatement(statement->as.blockStatement->statementArray.values[i], depth + 1);
            }
            break;
    }
}

// Macro to pretty print a binary expression
#define printBinaryExpression(expr, type)                  \
    do {                                                   \
        printf(#type "\n");                                \
        printIndent(depth + 1);                            \
        printf(KGRY "(left)\n" RESET);                     \
        printExpression(expr->leftExpression, depth + 2);  \
        printIndent(depth + 1);                            \
        printf(KGRY "(right)\n" RESET);                    \
        printExpression(expr->rightExpression, depth + 2); \
    } while (0)

#define printBinaryExpressionWithPunctuator(expr, type)          \
    do {                                                         \
        printf(#type KGRY "(punctuator=\"%.*s\")\n" RESET,       \
               expr->punctuator.length, expr->punctuator.start); \
        printIndent(depth + 1);                                  \
        printf(KGRY "(left)\n" RESET);                           \
        printExpression(expr->leftExpression, depth + 2);        \
        printIndent(depth + 1);                                  \
        printf(KGRY "(right)\n" RESET);                          \
        printExpression(expr->rightExpression, depth + 2);       \
    } while (0)

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
            printBinaryExpressionWithPunctuator(expression->as.additiveExpression, AdditiveExpression);
            break;
        }

        case MULTIPLICATIVE_EXPRESSION: {
            printBinaryExpressionWithPunctuator(expression->as.multiplicativeExpression, MultiplicativeExpression);
            break;
        }

        case UNARY_EXPRESSION: {
            UnaryExpression* unaryExpr = expression->as.unaryExpression;
            printf("UnaryExpression\n");
            printIndent(depth + 1);
            printf(KGRY "(punctuator=\"%.*s\")\n" RESET, unaryExpr->punctuator.length, unaryExpr->punctuator.start);
            printExpression(unaryExpr->rightExpression, depth + 2);
            break;
        }

        case COMPARISON_EXPRESSION: {
            printBinaryExpressionWithPunctuator(expression->as.comparisonExpression, ComparisonExpression);
            break;
        }

        case EQUALITY_EXPRESSION: {
            printBinaryExpressionWithPunctuator(expression->as.equalityExpression, EqualityExpression);
            break;
        }
        case LOGICAL_AND_EXPRESSION: {
            printBinaryExpression(expression->as.logicalAndExpression, LogicalAndExpression);
            break;
        }
        case LOGICAL_OR_EXPRESSION: {
            printBinaryExpression(expression->as.logicalOrExpression, LogicalOrExpression);
            break;
        }
        case BLOCK_EXPRESSION:
            printf("BlockExpression" KGRY "(numberOfStatements=%zu)\n" RESET, expression->as.blockExpression->statementArray.used);
            BlockExpression* blockExpr = expression->as.blockExpression;
            for (int i = 0; i < blockExpr->statementArray.used; i++) {
                printStatement(blockExpr->statementArray.values[i], depth + 1);
            }
            printExpression(blockExpr->lastExpression, depth + 1);
            break;
        case LAMBDA_EXPRESSION: {
            LambdaExpression* lambdaExpr = expression->as.lambdaExpression;

            printf("LambdaExpression\n");

            printIndent(depth + 1);
            printf("ParameterList" KGRY "(numberOfParameters=%zu)\n" RESET, lambdaExpr->parameters->used);
            for (int i = 0; i < lambdaExpr->parameters->used; i++) {
                printIndent(depth + 2);
                printf("IdentifierLiteral" KGRY "(token=\"%.*s\")\n" RESET, lambdaExpr->parameters->values[i].token.length, lambdaExpr->parameters->values[i].token.start);
            }

            Expression* blockExpr = (Expression*)malloc(sizeof(Expression));
            blockExpr->type = BLOCK_EXPRESSION;
            blockExpr->as.blockExpression = lambdaExpr->bodyBlock;
            printExpression(blockExpr, depth + 1);
            free(blockExpr);
            break;
        }
        case CALL_EXPRESSION: {
            CallExpression* callExpr = expression->as.callExpression;
            printf("CallExpression" KGRY "(numberOfArguments=%hhu)\n" RESET, callExpr->arguments->used);

            printExpression(callExpr->leftHandSide, depth + 1);

            printIndent(depth + 1);
            printf("Arguments" KGRY "(numberOfArguments=%hhu)\n" RESET, callExpr->arguments->used);
            for (int i = 0; i < callExpr->arguments->used; i++) {
                printExpression(callExpr->arguments->values[i], depth + 2);
            }
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
            printf("NumberLiteral" KGRY "(token=\"%.*s\")\n" RESET, literal->as.numberLiteral->token.length, literal->as.numberLiteral->token.start);
            break;

        case BOOLEAN_LITERAL:
            printf("BooleanLiteral" KGRY "(token=\"%.*s\")\n" RESET, literal->as.booleanLiteral->token.length, literal->as.numberLiteral->token.start);
            break;

        case IDENTIFIER_LITERAL:
            printf("IdentifierLiteral" KGRY "(token=\"%.*s\")\n" RESET, literal->as.identifierLiteral->token.length, literal->as.identifierLiteral->token.start);
            break;

        case STRING_LITERAL:
            printf("StringLiteral" KGRY "(token=%.*s)\n" RESET, literal->as.stringLiteral->token.length, literal->as.stringLiteral->token.start);
            break;

            // Add cases for other literal types
    }
}

// ---------------------------------------------------------------------------
// -------------------------------- COMPILER ---------------------------------
// ---------------------------------------------------------------------------

// New structure to keep track of functions to print later
typedef struct {
    Function** functions;
    size_t used;
    size_t size;
} FunctionArray;

static void initFunctionArray(FunctionArray* array) {
    array->functions = malloc(sizeof(Function*) * 8);
    array->used = 0;
    array->size = 8;
}

static void insertFunction(FunctionArray* array, Function* function) {
    if (array->used == array->size) {
        array->size *= 2;
        array->functions = realloc(array->functions, sizeof(Function*) * array->size);
    }
    array->functions[array->used++] = function;
}

static void freeFunctionArray(FunctionArray* array) {
    free(array->functions);
    array->functions = NULL;
    array->used = array->size = 0;
}

static void printConstantPool(ConstantPool constantPool, FunctionArray* arrayFunctionsToPrintLater) {
    for (size_t i = 0; i < constantPool.used; i++) {
        Constant value = constantPool.values[i];
        printf(KGRY " #%-3zu " RESET, i);
        switch (value.type) {
            case CONST_TYPE_DOUBLE:
                printf("(double) %f\n", value.as.number);
                break;
            case CONST_TYPE_STRING:
                printf("(string) \"%s\"\n", value.as.string);
                break;
            case CONST_TYPE_IDENTIFIER:
                printf("(identifier) \"%s\"\n", value.as.string);
                break;
            case CONST_TYPE_LAMBDA:
                printf("(function) <%p parameterCount=%d>\n",
                       (void*)value.as.lambda->code, value.as.lambda->parameterCount);
                insertFunction(arrayFunctionsToPrintLater, value.as.lambda);
                break;
        }
    }
}

static void printBytecodeArray(BytecodeArray bytecodeArray) {
    for (int i = 0; i < bytecodeArray.used; i++) {
        printf(KGRY " %-4d " RESET, i);
        switch (bytecodeArray.values[i].type) {
            case OP_LOAD_CONSTANT:
                printf("LOAD_CONSTANT #%zu\n", bytecodeArray.values[i].maybeOperand1);
                break;
            case OP_DEFINE_GLOBAL_VAL:
                printf("DEFINE_GLOBAL_VAL #%zu\n", bytecodeArray.values[i].maybeOperand1);
                break;
            case OP_GET_GLOBAL_VAL:
                printf("GET_GLOBAL_VAL #%zu\n", bytecodeArray.values[i].maybeOperand1);
                break;
            case OP_DEFINE_LOCAL_VAL_FAST:
                printf("DEFINE_LOCAL_VAL_FAST\n");
                break;
            case OP_GET_LOCAL_VAL_FAST:
                printf("GET_LOCAL_VAL_FAST #%zu\n", bytecodeArray.values[i].maybeOperand1);
                break;
            case OP_DEFINE_GLOBAL_VAR:
                printf("DEFINE_GLOBAL_VAR #%zu\n", bytecodeArray.values[i].maybeOperand1);
                break;
            case OP_GET_GLOBAL_VAR:
                printf("GET_GLOBAL_VAR #%zu\n", bytecodeArray.values[i].maybeOperand1);
                break;
            case OP_DEFINE_LOCAL_VAR_FAST:
                printf("DEFINE_LOCAL_VAR_FAST\n");
                break;
            case OP_GET_LOCAL_VAR_FAST:
                printf("GET_LOCAL_VAR_FAST #%zu\n", bytecodeArray.values[i].maybeOperand1);
                break;
            case OP_SET_GLOBAL_VAR:
                printf("SET_GLOBAL_VAR #%zu\n", bytecodeArray.values[i].maybeOperand1);
                break;
            case OP_SET_LOCAL_VAR_FAST:
                printf("SET_LOCAL_VAR_FAST #%zu\n", bytecodeArray.values[i].maybeOperand1);
                break;
            case OP_POPN:
                printf("POPN %zu\n", bytecodeArray.values[i].maybeOperand1);
                break;
            case OP_PRINT:
                printf("PRINT\n");
                break;
            case OP_TRUE:
                printf("TRUE\n");
                break;
            case OP_NULL:
                printf("NULL\n");
                break;
            case OP_FALSE:
                printf("FALSE\n");
                break;
            case OP_UNARY_NEGATE:
                printf("UNARY_NEGATE\n");
                break;
            case OP_UNARY_NOT:
                printf("UNARY_NOT\n");
                break;
            case OP_BINARY_ADD:
                printf("BINARY_ADD\n");
                break;
            case OP_BINARY_SUBTRACT:
                printf("BINARY_SUBTRACT\n");
                break;
            case OP_BINARY_MULTIPLY:
                printf("BINARY_MULTIPLY\n");
                break;
            case OP_BINARY_DIVIDE:
                printf("BINARY_DIVIDE\n");
                break;
            case OP_BINARY_GT:
                printf("BINARY_GT\n");
                break;
            case OP_BINARY_GTE:
                printf("BINARY_GTE\n");
                break;
            case OP_BINARY_LT:
                printf("BINARY_LT\n");
                break;
            case OP_BINARY_LTE:
                printf("BINARY_LTE\n");
                break;
            case OP_BINARY_LOGICAL_AND:
                printf("BINARY_LOGICAL_AND\n");
                break;
            case OP_BINARY_LOGICAL_OR:
                printf("BINARY_LOGICAL_OR\n");
                break;
            case OP_BINARY_EQUAL:
                printf("BINARY_EQUAL\n");
                break;
            case OP_BINARY_NOT_EQUAL:
                printf("BINARY_NOT_EQUAL\n");
                break;
            case OP_JUMP_IF_FALSE:
                printf("JUMP_IF_FALSE #%zu\n", bytecodeArray.values[i].maybeOperand1);
                break;
            case OP_JUMP:
                printf("JUMP #%zu\n", bytecodeArray.values[i].maybeOperand1);
                break;
            case OP_SWAP:
                printf("SWAP #%zu\n", bytecodeArray.values[i].maybeOperand1);
                break;
            case OP_LAMBDA:
                printf("LAMBDA #%zu\n", bytecodeArray.values[i].maybeOperand1);
                break;
            case OP_CALL:
                printf("CALL\n");
                break;
            case OP_RETURN:
                printf("RETURN\n");
                break;
        }
    }
}

void printCompiledCode(CompiledCode compiledCode) {
    printf(KBOLD KCYN "Compiled code\n" RESET KBOFF);
    printCompiledCodeObject(compiledCode.topLevelCodeObject, "main");
}

void printCompiledCodeObject(CompiledCodeObject compiledCodeObject, const char* name) {
    printf(KCYN "%s\n" RESET, name);
    FunctionArray functionsToPrint;
    initFunctionArray(&functionsToPrint);

    printConstantPool(compiledCodeObject.constantPool, &functionsToPrint);
    printf("\n");
    printBytecodeArray(compiledCodeObject.bytecodeArray);

    printf("\n");

    // Print all collected functions
    for (size_t i = 0; i < functionsToPrint.used; i++) {
        Function* function = functionsToPrint.functions[i];
        char functionName[32];
        snprintf(functionName, sizeof(functionName), "%p", (void*)function->code);
        printCompiledCodeObject(*(function->code), functionName);
    }

    freeFunctionArray(&functionsToPrint);
}

// ---------------------------------------------------------------------------
// ----------------------------------- VM ------------------------------------
// ---------------------------------------------------------------------------

// Print VM stack. The top of the stack will be on the left.
void printStack(const Value* topOfStack, const Value* bottomOfStack) {
    printf(KGRY "t[ " RESET);
    while (topOfStack != bottomOfStack) {
        topOfStack--;
        Value val = *topOfStack;
        switch (val.type) {
            case TYPE_BOOLEAN:
                printf(KGRY "{" RESET " %s " KGRY "} " RESET, val.as.booleanVal ? "true" : "false");
                break;
            case TYPE_DOUBLE:
                printf(KGRY "{" RESET " %.5f " KGRY "} " RESET, val.as.doubleVal);
                break;
            case TYPE_NULL:
                printf(KGRY "{" RESET " NULL " KGRY "} " RESET);
                break;
            case TYPE_STRING:
                printf(KGRY "{" RESET " %.10s " KGRY "} " RESET, val.as.stringVal);
                break;
            case TYPE_LAMBDA:
                printf(KGRY "{" RESET " %p " KGRY "} " RESET, val.as.lambdaVal);
                break;
        }
    }
    printf(KGRY "]b\n" RESET);
}