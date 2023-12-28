#include "compiler.h"

#include <stdlib.h>
#include <string.h>

#include "bytecode.h"
#include "config.h"
#include "debug.h"
#include "syntax.h"
#include "vm.h"

void initCompiler(Compiler* compiler) {
    BytecodeArray bytecodeArray;
    INIT_ARRAY(bytecodeArray, Bytecode);
    compiler->compiledBytecode = bytecodeArray;
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
 * Add a constant to the compiler's constant table.
 * These go alongwise the bytecode in the compiled code.
 * */
// static void addLiteralToConstantTable(Compiler* compiler) {
// }

/* VISITOR FUNCTIONS */

static void visitAdditiveExpression(Compiler* compiler, AdditiveExpression* additiveExpression) {
    visitExpression(compiler, additiveExpression->leftExpression);
    visitExpression(compiler, additiveExpression->rightExpression);

    switch (additiveExpression->punctuator->type) {
        case TOKEN_PLUS:
            emitBytecode(compiler, BYTECODE_ADD());
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

static void visitNumberLiteral(Compiler* compiler, NumberLiteral* numberLiteral) {
    Bytecode bytecode = BYTECODE_CONSTANT_DOUBLE(tokenTodouble(numberLiteral->token));
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

        default:
            fprintf(stderr, "Unimplemented statement type %d.", statement->type);
            exit(EXIT_FAILURE);
            break;
    }
}

BytecodeArray compileAST(Compiler* compiler, Source ASTSource) {
    for (int i = 0; i < ASTSource.numberOfStatements; i++) {
        Statement* statement = &(ASTSource.rootStatements[i]);
        visitStatement(compiler, statement);
    }

    if (DEBUG_COMPILER) {
        printBytecodeArray(compiler->compiledBytecode);
    }

    return compiler->compiledBytecode;
}
