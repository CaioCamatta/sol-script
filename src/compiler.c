#include "compiler.h"

#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "bytecode.h"
#include "config.h"
#include "debug.h"
#include "parser.h"
#include "syntax.h"
#include "vm.h"

void initCompiler(Compiler* compiler, Source* ASTSource) {
    BytecodeArray bytecodeArray;
    INIT_ARRAY(bytecodeArray, Bytecode);
    ConstantPool constantPool;
    INIT_ARRAY(constantPool, Constant);
    compiler->compiledBytecode = bytecodeArray;
    compiler->ASTSource = ASTSource;
    compiler->constantPool = constantPool;
    compiler->currentStackHeight = 0;
}

/* FORWARD DECLARATIONS */

static void visitStatement(Compiler* compiler, Statement* statement);
static void visitExpression(Compiler* compiler, Expression* expression);
static void visitLiteral(Compiler* compiler, Literal* literal);

/* UTILITIES */

/**
 * Print error and crash.
 *
 * TODO: make it so we don't crash completely. It should be simple to print and error, move to compiling the next statement,
 * then after all statements have been compiled print all errors and exit the program.
 */
#define errorAndExit(...)             \
    {                                 \
        fprintf(stderr, __VA_ARGS__); \
        exit(EXIT_FAILURE);           \
    }

// Add bytecode to the compiled program.
static void emitBytecode(Compiler* compiler, Bytecode bytecode) {
    INSERT_ARRAY(compiler->compiledBytecode, bytecode, Bytecode);
}

/**
 * The compiler can know in advance how tall the VM stack will be at any point. This function helps keep track
 * of the height. For example, a local variable declaration increases the stack by 1 (locals live on the stack).
 */
static void increaseStackHeight(Compiler* compiler) {
    if (compiler->currentStackHeight == UCHAR_MAX) {
        errorAndExit("Compiler error: VM stack will overflow.");
    }

    compiler->currentStackHeight++;
}

/**
 * The compiler can know in advance how tall the VM stack will be at any point. This function helps keep track
 * of the height. For example, an addition decreases the height of the stack by 1 (pop, pop, push).
 */
static void decreaseStackHeight(Compiler* compiler) {
    compiler->currentStackHeight--;
}

static double tokenTodouble(Token token) {
    char substring[token.length + 1];
    memcpy(substring, token.start, token.length);
    substring[token.length] = '\0';
    return atof(token.start);
}

// Copy a string from the input code to the heap and make it null-terminated so it can be used by the VM.
char* copyStringToHeap(const char* chars, int length) {
    // TODO: consider adding the hash here to potentially save time in the VM
    char* heapString = malloc(sizeof(char) * (length + 1));
    memcpy(heapString, chars, length);
    heapString[length] = '\0';
    return heapString;
}

/**
 * Given a new constant, try to find its index in the constant pool. Returns -1 if not in pool or the index.
 *
 * TODO: this method is terribly inefficient as it iterates through the entire table. We should keep a hash table
 * of (number -> index in table) on the side and discard it after compilation is done.
 */
static size_t findConstantInPool(Compiler* compiler, Constant constant) {
    for (size_t i = 0; i < compiler->constantPool.used; i++) {
        // Skip if its not the same type
        if (constant.type != compiler->constantPool.values[i].type) continue;

        // Check if value is the same
        switch (compiler->constantPool.values[i].type) {
            case CONST_TYPE_STRING:
            case CONST_TYPE_IDENTIFIER:
                if (strcmp(constant.as.string, compiler->constantPool.values[i].as.string) == 0) return i;
                break;

            case CONST_TYPE_DOUBLE:
                if (constant.as.number == compiler->constantPool.values[i].as.number) return i;
                break;
        }
    }
    return -1;
}

/**
 * Add a constant to the compiler's constant pool, returns index in the pool.
 * If the constant is already in the pool, the index of the existing one is returned.
 * (These constants go alongwise the bytecode in the compiled code.)
 * */
static size_t addConstantToPool(Compiler* compiler, Constant constant) {
    // Check if its already there
    size_t maybeIndexInPool = findConstantInPool(compiler, constant);
    if (maybeIndexInPool != -1) return maybeIndexInPool;

    // If not, insert and return index
    INSERT_ARRAY(compiler->constantPool, constant, Constant);
    return compiler->constantPool.used - 1;
}

/* VISITOR FUNCTIONS */

static void visitAdditiveExpression(Compiler* compiler, AdditiveExpression* additiveExpression) {
    visitExpression(compiler, additiveExpression->leftExpression);
    visitExpression(compiler, additiveExpression->rightExpression);

    switch (additiveExpression->punctuator.type) {
        case TOKEN_PLUS:
            emitBytecode(compiler, BYTECODE(OP_BINARY_ADD));
            break;
        case TOKEN_MINUS:
            emitBytecode(compiler, BYTECODE(OP_BINARY_SUBTRACT));
            break;
        default:
            break;
    }
    decreaseStackHeight(compiler);
}

static void visitMultiplicativeExpression(Compiler* compiler, MultiplicativeExpression* multiplicativeExpression) {
    visitExpression(compiler, multiplicativeExpression->leftExpression);
    visitExpression(compiler, multiplicativeExpression->rightExpression);

    switch (multiplicativeExpression->punctuator.type) {
        case TOKEN_STAR:
            emitBytecode(compiler, BYTECODE(OP_BINARY_MULTIPLY));
            break;
        case TOKEN_SLASH:
            emitBytecode(compiler, BYTECODE(OP_BINARY_DIVIDE));
            break;
        default:
            break;
    }
    decreaseStackHeight(compiler);
}

static void visitEqualityExpression(Compiler* compiler, EqualityExpression* equalityExpression) {
    visitExpression(compiler, equalityExpression->leftExpression);
    visitExpression(compiler, equalityExpression->rightExpression);

    switch (equalityExpression->punctuator.type) {
        case TOKEN_EQUAL_EQUAL:
            emitBytecode(compiler, BYTECODE(OP_BINARY_EQUAL));
            break;
        case TOKEN_EXCLAMATION_EQUAL:
            emitBytecode(compiler, BYTECODE(OP_BINARY_NOT_EQUAL));
            break;
        default:
            break;
    }
    decreaseStackHeight(compiler);
}

static void visitLogicalOrExpression(Compiler* compiler, LogicalOrExpression* logicalOrExpression) {
    visitExpression(compiler, logicalOrExpression->leftExpression);
    visitExpression(compiler, logicalOrExpression->rightExpression);
    emitBytecode(compiler, BYTECODE(OP_BINARY_LOGICAL_OR));
    decreaseStackHeight(compiler);
}

static void visitLogicalAndExpression(Compiler* compiler, LogicalAndExpression* logicalAndExpression) {
    visitExpression(compiler, logicalAndExpression->leftExpression);
    visitExpression(compiler, logicalAndExpression->rightExpression);
    emitBytecode(compiler, BYTECODE(OP_BINARY_LOGICAL_AND));
    decreaseStackHeight(compiler);
}

// Visit the two expression on left and write, emit bytecode to add them
static void visitComparisonExpression(Compiler* compiler, ComparisonExpression* comparisonExpression) {
    visitExpression(compiler, comparisonExpression->leftExpression);
    visitExpression(compiler, comparisonExpression->rightExpression);

    switch (comparisonExpression->punctuator.type) {
        case TOKEN_GREATER:
            emitBytecode(compiler, BYTECODE(OP_BINARY_GT));
            break;
        case TOKEN_GREATER_EQUAL:
            emitBytecode(compiler, BYTECODE(OP_BINARY_GTE));
            break;
        case TOKEN_LESSER:
            emitBytecode(compiler, BYTECODE(OP_BINARY_LT));
            break;
        case TOKEN_LESSER_EQUAL:
            emitBytecode(compiler, BYTECODE(OP_BINARY_LTE));
            break;
        default:
            break;
    }
    decreaseStackHeight(compiler);
}

static void visitUnaryExpression(Compiler* compiler, UnaryExpression* unaryExpression) {
    visitExpression(compiler, unaryExpression->rightExpression);
    if (unaryExpression->punctuator.type == TOKEN_MINUS) {
        emitBytecode(compiler, BYTECODE(OP_UNARY_NEGATE));
    } else if (unaryExpression->punctuator.type == TOKEN_EXCLAMATION) {
        emitBytecode(compiler, BYTECODE(OP_UNARY_NOT));
    }
}

static void visitPrimaryExpression(Compiler* compiler, PrimaryExpression* primaryExpression) {
    visitLiteral(compiler, primaryExpression->literal);
}

// Visit the expression. (Should be executed only for its side-effects)
static void visitExpressionStatement(Compiler* compiler, ExpressionStatement* expressionStatement) {
    visitExpression(compiler, expressionStatement->expression);
}

/**
 * Visit the expression after the val declaration, save the variable identifier to constant pool,
 * emit instruction to set identifier = val
 */
static void visitValDeclarationStatement(Compiler* compiler, ValDeclarationStatement* valDeclarationStatement) {
    visitExpression(compiler, valDeclarationStatement->expression);

    Constant constant = IDENTIFIER_CONST(copyStringToHeap(valDeclarationStatement->identifier->token.start,
                                                          valDeclarationStatement->identifier->token.length));

    // TODO: Remove duplicated check; addConstantToPool already runs findConstantInPool.
    if (findConstantInPool(compiler, constant) != -1) errorAndExit("Error: val \"%s\" is already declared. Redeclaration is not permitted.", constant.as.string);

    size_t constantIndex = addConstantToPool(compiler, constant);
    emitBytecode(compiler, BYTECODE_OPERAND_1(OP_SET_VAL, constantIndex));

    increaseStackHeight(compiler);
}

// Visit expression following print, then emit bytecode to print that expression
static void visitPrintStatement(Compiler* compiler, PrintStatement* printStatement) {
    visitExpression(compiler, printStatement->expression);
    emitBytecode(compiler, BYTECODE(OP_PRINT));
}

static void visitBlockStatement(Compiler* compiler, BlockStatement* blockStatement) {
    // Keep track of the stack height so we can later pop all the variables etc defined in it.
    uint8_t stackHeightBeforeBlockStmt = compiler->currentStackHeight;

    for (size_t i = 0; i < blockStatement->statementArray.used; i++) {
        Statement* statement = blockStatement->statementArray.values[i];
        visitStatement(compiler, statement);
    }

    // Pop all the Values that were put in the stack in the block.
    uint8_t stackHeightAfterBlockStmt = compiler->currentStackHeight;
    uint8_t blockStmtStackEffect = stackHeightAfterBlockStmt - stackHeightBeforeBlockStmt;
    emitBytecode(compiler, BYTECODE_OPERAND_1(OP_POPN, blockStmtStackEffect));
}

// Add number to constant pool and emit bytecode to load it onto the stack
static void visitNumberLiteral(Compiler* compiler, NumberLiteral* numberLiteral) {
    double number = tokenTodouble(numberLiteral->token);
    Constant constant = DOUBLE_CONST(number);
    size_t constantIndex = addConstantToPool(compiler, constant);

    Bytecode bytecode = BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, constantIndex);
    emitBytecode(compiler, bytecode);

    increaseStackHeight(compiler);
}

static void visitBooleanLiteral(Compiler* compiler, BooleanLiteral* booleanLiteral) {
    switch (booleanLiteral->token.type) {
        case TOKEN_FALSE:
            emitBytecode(compiler, BYTECODE(OP_FALSE));
            break;
        case TOKEN_TRUE:
            emitBytecode(compiler, BYTECODE(OP_TRUE));
            break;
        default:
            errorAndExit("Error: failed to parse boolean from token '%.*s'.", booleanLiteral->token.length, booleanLiteral->token.start);
            return;
    }
    increaseStackHeight(compiler);
}

static void visitIdentifierLiteral(Compiler* compiler, IdentifierLiteral* identifierLiteral) {
    // Find address of this identifier in the constant pool
    char* identifierNameNullTerminated = strndup(identifierLiteral->token.start, identifierLiteral->token.length);
    Constant constant = (Constant){
        .type = CONST_TYPE_IDENTIFIER,
        .as = {identifierNameNullTerminated}};
    size_t index = findConstantInPool(compiler, constant);
    if (index == -1) errorAndExit("Error: identifier '%s' referenced before declaration.", identifierNameNullTerminated);

    // Generate bytecode to get the variable
    Bytecode bytecodeToGetVariable = BYTECODE_OPERAND_1(OP_GET_VAL, index);
    emitBytecode(compiler, bytecodeToGetVariable);

    increaseStackHeight(compiler);
}

static void visitStringLiteral(Compiler* compiler, StringLiteral* stringLiteral) {
    // Remove "'s from left and right of the string
    char* stringTrimmedAndNullTerminated = strndup(stringLiteral->token.start + 1, stringLiteral->token.length - 2);
    Constant constant = (Constant){
        .type = CONST_TYPE_STRING,
        .as = {stringTrimmedAndNullTerminated}};
    size_t constantIndex = addConstantToPool(compiler, constant);

    Bytecode bytecodeToGetVariable = BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, constantIndex);
    emitBytecode(compiler, bytecodeToGetVariable);

    increaseStackHeight(compiler);
}

static void visitLiteral(Compiler* compiler, Literal* literal) {
    switch (literal->type) {
        case NUMBER_LITERAL:
            visitNumberLiteral(compiler, literal->as.numberLiteral);
            break;
        case IDENTIFIER_LITERAL:
            visitIdentifierLiteral(compiler, literal->as.identifierLiteral);
            break;
        case STRING_LITERAL:
            visitStringLiteral(compiler, literal->as.stringLiteral);
            break;
        case BOOLEAN_LITERAL:
            visitBooleanLiteral(compiler, literal->as.booleanLiteral);
            break;
        default:
            fprintf(stderr, "Unimplemented literal type %d.", literal->type);
            exit(EXIT_FAILURE);
            break;
    }
}

static void visitExpression(Compiler* compiler, Expression* expression) {
    switch (expression->type) {
        case ADDITIVE_EXPRESSION:
            visitAdditiveExpression(compiler, expression->as.additiveExpression);
            break;
        case MULTIPLICATIVE_EXPRESSION:
            visitMultiplicativeExpression(compiler, expression->as.multiplicativeExpression);
            break;
        case UNARY_EXPRESSION:
            visitUnaryExpression(compiler, expression->as.unaryExpression);
            break;
        case PRIMARY_EXPRESSION:
            visitPrimaryExpression(compiler, expression->as.primaryExpression);
            break;
        case EQUALITY_EXPRESSION:
            visitEqualityExpression(compiler, expression->as.equalityExpression);
            break;
        case LOGICAL_OR_EXPRESSION:
            visitLogicalOrExpression(compiler, expression->as.logicalOrExpression);
            break;
        case LOGICAL_AND_EXPRESSION:
            visitLogicalAndExpression(compiler, expression->as.logicalAndExpression);
            break;
        case COMPARISON_EXPRESSION:
            visitComparisonExpression(compiler, expression->as.comparisonExpression);
            break;
        default:
            fprintf(stderr, "Unimplemented expression type %d.", expression->type);
            exit(EXIT_FAILURE);
            break;
    }
}

static void visitStatement(Compiler* compiler, Statement* statement) {
    switch (statement->type) {
        case EXPRESSION_STATEMENT:
            visitExpressionStatement(compiler, statement->as.expressionStatement);
            break;
        case VAL_DECLARATION_STATEMENT:
            visitValDeclarationStatement(compiler, statement->as.valDeclarationStatement);
            break;
        case PRINT_STATEMENT:
            visitPrintStatement(compiler, statement->as.printStatement);
            break;
        case BLOCK_STATEMENT:
            visitBlockStatement(compiler, statement->as.blockStatement);
            break;
        default:
            fprintf(stderr, "Unimplemented statement type %d.\n", statement->type);
            exit(EXIT_FAILURE);
            break;
    }
}

CompiledCode compile(Compiler* compiler) {
    clock_t startTime = clock();
    printf("Started compiling.\n");

    for (int i = 0; i < compiler->ASTSource->numberOfStatements; i++) {
        Statement* statement = compiler->ASTSource->rootStatements[i];
        visitStatement(compiler, statement);
    }

    // if (DEBUG_COMPILER) {
    //     printBytecodeArray(compiler->compiledBytecode);
    // }

    clock_t endTime = clock();
    double timeTaken = ((double)(endTime - startTime)) / CLOCKS_PER_SEC;
    printf("Done compiling in %.5f seconds.\n\n", timeTaken);

    CompiledCode code = (CompiledCode){
        .bytecodeArray = compiler->compiledBytecode,
        .constantPool = compiler->constantPool};

    return code;
}