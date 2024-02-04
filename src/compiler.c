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

/**
 * Add a constant to the compiler's constant pool, returns index in the pool.
 * These constants go alongwise the bytecode in the compiled code.
 * */
static size_t addConstantToPool(Compiler* compiler, Constant constant) {
    INSERT_ARRAY(compiler->constantPool, constant, Constant);
    return compiler->constantPool.used - 1;
}

/* VISITOR FUNCTIONS */

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
    // TODO: expand so it handles other types (like strings)
    visitLiteral(compiler, primaryExpression->literal);
}

static void visitExpressionStatement(Compiler* compiler, ExpressionStatement* expressionStatement) {
    visitExpression(compiler, expressionStatement->expression);
}

static void visitPrintStatement(Compiler* compiler, PrintStatement* printStatement) {
    visitExpression(compiler, printStatement->expression);
    emitBytecode(compiler, BYTECODE(OP_PRINT));
}

static void visitNumberLiteral(Compiler* compiler, NumberLiteral* numberLiteral) {
    double number = tokenTodouble(numberLiteral->token);
    Constant constant = DOUBLE_CONST(number);
    size_t constantIndex = addConstantToPool(compiler, constant);

    Bytecode bytecode = BYTECODE_CONSTANT_1(OP_LOAD_CONSTANT, constantIndex);
    emitBytecode(compiler, bytecode);
}

static void visitLiteral(Compiler* compiler, Literal* literal) {
    switch (literal->type) {
        case NUMBER_LITERAL:
            visitNumberLiteral(compiler, literal->as.numberLiteral);
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
            // TEMPORARY: just compile the expression.
            ExpressionStatement* expressionStatement = &(ExpressionStatement){
                .expression = statement->as.valDeclarationStatement->expression,
            };
            visitExpressionStatement(compiler, expressionStatement);
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