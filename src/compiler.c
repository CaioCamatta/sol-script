#include "compiler.h"

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

// Visit the two expression on left and write, emit bytecode to add them
static void visitAdditiveExpression(Compiler* compiler, AdditiveExpression* additiveExpression) {
    visitExpression(compiler, additiveExpression->leftExpression);
    visitExpression(compiler, additiveExpression->rightExpression);

    switch (additiveExpression->punctuator.type) {
        case TOKEN_PLUS:
            emitBytecode(compiler, BYTECODE(OP_ADD));
            break;

        default:
            break;
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

    Constant constant = STRING_CONST(copyStringToHeap(valDeclarationStatement->identifier->token.start,
                                                      valDeclarationStatement->identifier->token.length));
    size_t constantIndex = addConstantToPool(compiler, constant);
    emitBytecode(compiler, BYTECODE_CONSTANT_1(OP_SET_VAL, constantIndex));
}

// Visit expression following print, then emit bytecode to print that expression
static void visitPrintStatement(Compiler* compiler, PrintStatement* printStatement) {
    visitExpression(compiler, printStatement->expression);
    emitBytecode(compiler, BYTECODE(OP_PRINT));
}

// Add number to constant pool and emit bytecode to load it onto the stack
static void visitNumberLiteral(Compiler* compiler, NumberLiteral* numberLiteral) {
    double number = tokenTodouble(numberLiteral->token);
    Constant constant = DOUBLE_CONST(number);
    size_t constantIndex = addConstantToPool(compiler, constant);

    Bytecode bytecode = BYTECODE_CONSTANT_1(OP_LOAD_CONSTANT, constantIndex);
    emitBytecode(compiler, bytecode);
}

static void visitIdentifierLiteral(Compiler* compiler, IdentifierLiteral* identifierLiteral) {
    // Find address of this identifier in the constant pool
    char* identifierNameNullTerminated = strndup(identifierLiteral->token.start, identifierLiteral->token.length);
    Constant tempConstant = (Constant){
        .type = CONST_TYPE_STRING,
        .as = {identifierNameNullTerminated}};
    size_t index = findConstantInPool(compiler, tempConstant);
    if (index == -1) errorAndExit("Error: identifier '%s' referenced before declaration.", identifierNameNullTerminated);

    // Generate bytecode to get the global variable
    Bytecode getGlobal = BYTECODE_CONSTANT_1(OP_GET_VAL, index);
    emitBytecode(compiler, getGlobal);
}

static void visitLiteral(Compiler* compiler, Literal* literal) {
    switch (literal->type) {
        case NUMBER_LITERAL:
            visitNumberLiteral(compiler, literal->as.numberLiteral);
            break;
        case IDENTIFIER_LITERAL:
            visitIdentifierLiteral(compiler, literal->as.identifierLiteral);
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

        case PRIMARY_EXPRESSION:
            visitPrimaryExpression(compiler, expression->as.primaryExpression);
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

        case VAL_DECLARATION_STATEMENT: {
            visitValDeclarationStatement(compiler, statement->as.valDeclarationStatement);
            break;
        }
        case PRINT_STATEMENT:
            visitPrintStatement(compiler, statement->as.printStatement);
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