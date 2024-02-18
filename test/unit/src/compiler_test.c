#include "compiler.h"

#include <string.h>

#include "../minunit.h"
#include "debug.h"
#include "syntax.h"

// -------------------------------------------------------------------------------
// ---------------------- Macros to facilitate creating AST ----------------------
// -------------------------------------------------------------------------------

#define NUMBER_LITERAL(numberAsString)                                                                           \
    &(Literal) {                                                                                                 \
        .type = NUMBER_LITERAL,                                                                                  \
        .as.numberLiteral = &(NumberLiteral) {                                                                   \
            .token = (Token) { .type = TOKEN_NUMBER, .start = numberAsString, .length = strlen(numberAsString) } \
        }                                                                                                        \
    }

#define IDENTIFIER_LITERAL(name)                                                                 \
    &(Literal) {                                                                                 \
        .type = IDENTIFIER_LITERAL,                                                              \
        .as.identifierLiteral = &(IdentifierLiteral) {                                           \
            .token = (Token) { .type = TOKEN_IDENTIFIER, .start = name, .length = strlen(name) } \
        }                                                                                        \
    }

#define PRIMARY_EXPRESSION(literalInput)               \
    &(Expression) {                                    \
        .type = PRIMARY_EXPRESSION,                    \
        .as.primaryExpression = &(PrimaryExpression) { \
            .literal = literalInput                    \
        }                                              \
    }

#define ADDITIVE_EXPRESSION(left, operatorToken, right)  \
    &(Expression) {                                      \
        .type = ADDITIVE_EXPRESSION,                     \
        .as.additiveExpression = &(AdditiveExpression) { \
            .leftExpression = left,                      \
            .rightExpression = right,                    \
            .punctuator = operatorToken                  \
        }                                                \
    }

#define PRINT_STATEMENT(expressionInput)         \
    &(Statement) {                               \
        .type = PRINT_STATEMENT,                 \
        .as.printStatement = &(PrintStatement) { \
            .expression = expressionInput        \
        }                                        \
    }

#define VAL_DECLARATION_STATEMENT(identifierName, expressionInput)                                                      \
    &(Statement) {                                                                                                      \
        .type = VAL_DECLARATION_STATEMENT,                                                                              \
        .as.valDeclarationStatement = &(ValDeclarationStatement) {                                                      \
            .identifier = &(IdentifierLiteral){                                                                         \
                .token = (Token){.type = TOKEN_IDENTIFIER, .start = identifierName, .length = strlen(identifierName)}}, \
            .expression = expressionInput                                                                               \
        }                                                                                                               \
    }

#define EXPRESSION_STATEMENT(expressionInput)              \
    &(Statement) {                                         \
        .type = EXPRESSION_STATEMENT,                      \
        .as.expressionStatement = &(ExpressionStatement) { \
            .expression = expressionInput                  \
        }                                                  \
    }

#define TOKEN_PLUS() \
    (Token) { .type = TOKEN_PLUS, .start = "+", .length = 1 }

// ------------------------------------------------------------------------
// ---------------------------- Test utilities ----------------------------
// ------------------------------------------------------------------------

// Compare two bytecode arrays. Returns 1 if equal, 0 if not
static int compareTypesInBytecodeArrays(BytecodeArray expected, BytecodeArray actual) {
    if (expected.used != actual.used) {
        return 0;
    }

    for (size_t i = 0; i < expected.used; ++i) {
        if (expected.values[i].type != actual.values[i].type) {
            return 0;
        }
    }

    return 1;
}

// -------------------------------------------------------------------------
// --------------------------------- Tests ---------------------------------
// -------------------------------------------------------------------------

// Test basic arithmetic expression
int test_compiler() {
    Compiler compiler;

    Source testSource = {
        .rootStatements = {
            EXPRESSION_STATEMENT(
                ADDITIVE_EXPRESSION(
                    PRIMARY_EXPRESSION(NUMBER_LITERAL("5")),
                    TOKEN_PLUS(),
                    PRIMARY_EXPRESSION(NUMBER_LITERAL("7"))))},
        .numberOfStatements = 1,
    };

    initCompiler(&compiler, &testSource);
    CompiledCode compiledCode = compile(&compiler);
    printCompiledCode(compiledCode);

    // Expected bytecode
    Bytecode expectedBytecode[] = {
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_BINARY_ADD},
    };
    BytecodeArray expectedBytecodeArray = {.values = expectedBytecode, .used = 3};

    // Compared the actual and expected
    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.bytecodeArray));
    ASSERT(compiledCode.constantPool.values[0].as.number == 5);            // 5 is in slot 0 of the pool
    ASSERT(compiledCode.bytecodeArray.values[0].maybeConstantIndex == 0);  // first instruction loads slot 0
    ASSERT(compiledCode.constantPool.values[1].as.number == 7);
    ASSERT(compiledCode.bytecodeArray.values[1].maybeConstantIndex == 1);

    // Clean up
    FREE_ARRAY(compiledCode.bytecodeArray);
    FREE_ARRAY(compiledCode.constantPool);

    return SUCCESS_RETURN_CODE;
}

int test_compiler_print() {
    Compiler compiler;

    // Set up a source structure with a print statement
    Source testSource = {
        .rootStatements = {
            PRINT_STATEMENT(PRIMARY_EXPRESSION(NUMBER_LITERAL("42")))},
        .numberOfStatements = 1,
    };

    // Initialize and run the compiler
    initCompiler(&compiler, &testSource);
    CompiledCode compiledCode = compile(&compiler);

    // Verify the bytecode
    // Assuming the bytecode for a print statement is OP_PRINT followed by the value to print
    ASSERT(compiledCode.bytecodeArray.used == 2);                           // Check if two bytecode instructions are generated
    ASSERT(compiledCode.bytecodeArray.values[0].type == OP_LOAD_CONSTANT);  // First should be OP_CONSTANT
    ASSERT(compiledCode.bytecodeArray.values[1].type == OP_PRINT);          // Second should be OP_PRINT

    // Optionally, print the bytecode for visual verification
    printCompiledCode(compiledCode);

    // Clean up
    FREE_ARRAY(compiledCode.bytecodeArray);
    FREE_ARRAY(compiledCode.constantPool);

    return SUCCESS_RETURN_CODE;
}

int test_compiler_val_declaration() {
    Compiler compiler;

    // Setup a source structure with a val declaration statement
    Source testSource = {
        .rootStatements = {
            VAL_DECLARATION_STATEMENT("x", PRIMARY_EXPRESSION(NUMBER_LITERAL("42")))},
        .numberOfStatements = 1,
    };

    initCompiler(&compiler, &testSource);
    CompiledCode compiledCode = compile(&compiler);

    // Verify bytecode for val declaration
    ASSERT(compiledCode.bytecodeArray.used == 2);                           // Check if two bytecode instructions are generated
    ASSERT(compiledCode.bytecodeArray.values[0].type == OP_LOAD_CONSTANT);  // First should be OP_LOAD_CONSTANT
    ASSERT(compiledCode.bytecodeArray.values[1].type == OP_SET_VAL);        // Second should be OP_SET_VAL

    printCompiledCode(compiledCode);

    FREE_ARRAY(compiledCode.bytecodeArray);
    FREE_ARRAY(compiledCode.constantPool);

    return SUCCESS_RETURN_CODE;
}

int test_compiler_variable_declaration_and_printing() {
    // val x = 42;
    // print x;
    Source testSource = {
        .rootStatements = {
            VAL_DECLARATION_STATEMENT("x", PRIMARY_EXPRESSION(NUMBER_LITERAL("42"))),
            PRINT_STATEMENT(PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("x")))},
        .numberOfStatements = 2,
    };

    Compiler compiler;
    initCompiler(&compiler, &testSource);
    CompiledCode compiledCode = compile(&compiler);

    ASSERT(compiledCode.bytecodeArray.values[0].type == OP_LOAD_CONSTANT);  // load 42 into stack
    ASSERT(compiledCode.bytecodeArray.values[1].type == OP_SET_VAL);        // assign top of stack to variable 'x'
    ASSERT(compiledCode.bytecodeArray.values[2].type == OP_GET_VAL);        // get variable 'x'
    ASSERT(compiledCode.bytecodeArray.values[3].type == OP_PRINT);          // print top of stack

    ASSERT(compiledCode.constantPool.values[0].type == CONST_TYPE_DOUBLE);  // 42
    ASSERT(compiledCode.constantPool.values[0].as.number == 42);
    ASSERT(compiledCode.constantPool.values[1].type == CONST_TYPE_STRING);  // x

    FREE_ARRAY(compiledCode.bytecodeArray);
    FREE_ARRAY(compiledCode.constantPool);

    return SUCCESS_RETURN_CODE;  // Assuming SUCCESS_RETURN_CODE is defined as part of your testing framework
}

int test_add_constant_to_pool_no_duplicates() {
    Source source = (Source){
        .rootStatements = {
            VAL_DECLARATION_STATEMENT("x", PRIMARY_EXPRESSION(NUMBER_LITERAL("42"))),
            PRINT_STATEMENT(PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("x"))),
            VAL_DECLARATION_STATEMENT("y", PRIMARY_EXPRESSION(NUMBER_LITERAL("42")))},
        .numberOfStatements = 3,
    };

    Compiler compiler;
    initCompiler(&compiler, &source);
    CompiledCode compiledCode = compile(&compiler);

    // Assert the constants were not added twice; there should be one for '42', one for 'x', one for 'y'.
    ASSERT(compiledCode.constantPool.used == 3);

    // Cleanup
    FREE_ARRAY(compiler.compiledBytecode);
    FREE_ARRAY(compiler.constantPool);

    return SUCCESS_RETURN_CODE;
}