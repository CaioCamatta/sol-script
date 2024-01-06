#include "compiler.h"

#include "../minunit.h"
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

        // Add more checks here
    }

    return 1;
}

// Test basic arithmetic expression
int test_compiler() {
    Compiler compiler;
    initCompiler(&compiler);

    Source testSource = {
        .rootStatements = &(Statement){
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

        },
        .numberOfStatements = 1,
    };

    BytecodeArray compiledBytecode = compileAST(&compiler, testSource);

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