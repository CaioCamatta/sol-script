#include "compiler.h"

#include "../minunit.h"
#include "debug.h"
#include "syntax.h"

// Compare two bytecode arrays
static int compareBytecodeArrays(BytecodeArray expected, BytecodeArray actual) {
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

// Test basic arithmetic expression
int test_compiler() {
    Compiler compiler;

    Source testSource = {
        .rootStatements = {&(Statement){
            .type = EXPRESSION_STATEMENT,
            .as.expressionStatement = &(ExpressionStatement){
                .expression = &(Expression){
                    .type = ADDITIVE_EXPRESSION,
                    .as.additiveExpression = &(AdditiveExpression){
                        .leftExpression = &(Expression){
                            .type = PRIMARY_EXPRESSION,
                            .as.primaryExpression = &(PrimaryExpression){
                                .literal = &(Literal){
                                    .type = NUMBER_LITERAL,
                                    .as.numberLiteral = &(NumberLiteral){
                                        .token = {.type = TOKEN_NUMBER, .start = "5", .length = 1},
                                    },
                                },
                            },
                        },
                        .rightExpression = &(Expression){
                            .type = PRIMARY_EXPRESSION,
                            .as.primaryExpression = &(PrimaryExpression){
                                .literal = &(Literal){
                                    .type = NUMBER_LITERAL,
                                    .as.numberLiteral = &(NumberLiteral){
                                        .token = {.type = TOKEN_NUMBER, .start = "7", .length = 1},
                                    },
                                },
                            },
                        },
                        .punctuator = &(Token){.type = TOKEN_PLUS, .start = "+", .length = 1},
                    },
                },
            },

        }},
        .numberOfStatements = 1,
    };

    initCompiler(&compiler, &testSource);
    BytecodeArray compiledBytecode = compile(&compiler);
    printBytecodeArray(compiledBytecode);

    // Expected bytecode
    Bytecode expectedBytecode[] = {
        {.type = OP_CONSTANT},
        {.type = OP_CONSTANT},
        {.type = OP_ADD},
    };
    BytecodeArray expectedBytecodeArray = {.values = expectedBytecode, .used = 3};

    // Compared the actual and expected
    ASSERT(compareBytecodeArrays(expectedBytecodeArray, compiledBytecode));

    return SUCCESS_RETURN_CODE;
}

int test_compiler_print() {
    Compiler compiler;

    // Set up a source structure with a print statement
    Source testSource = {
        .rootStatements = {&(Statement){
            .type = PRINT_STATEMENT,
            .as.printStatement = &(PrintStatement){
                .expression = &(Expression){
                    .type = PRIMARY_EXPRESSION,
                    .as.primaryExpression = &(PrimaryExpression){
                        .literal = &(Literal){
                            .type = NUMBER_LITERAL,
                            .as.numberLiteral = &(NumberLiteral){
                                .token = (Token){.type = TOKEN_NUMBER, .start = "42", .length = 2}}}}}}}},
        .numberOfStatements = 1,
    };

    // Initialize and run the compiler
    initCompiler(&compiler, &testSource);
    BytecodeArray compiledBytecode = compile(&compiler);

    // Verify the bytecode
    // Assuming the bytecode for a print statement is OP_PRINT followed by the value to print
    ASSERT(compiledBytecode.used == 2);                      // Check if two bytecode instructions are generated
    ASSERT(compiledBytecode.values[0].type == OP_CONSTANT);  // First should be OP_CONSTANT
    ASSERT(compiledBytecode.values[1].type == OP_PRINT);     // Second should be OP_PRINT

    // Optionally, print the bytecode for visual verification
    printBytecodeArray(compiledBytecode);

    // Clean up
    FREE_ARRAY(compiledBytecode);

    return SUCCESS_RETURN_CODE;
}