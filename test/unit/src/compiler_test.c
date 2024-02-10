#include "compiler.h"

#include "../minunit.h"
#include "debug.h"
#include "syntax.h"

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
                        .punctuator = (Token){.type = TOKEN_PLUS, .start = "+", .length = 1},
                    },
                },
            },

        }},
        .numberOfStatements = 1,
    };

    initCompiler(&compiler, &testSource);
    CompiledCode compiledCode = compile(&compiler);
    printCompiledCode(compiledCode);

    // Expected bytecode
    Bytecode expectedBytecode[] = {
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_ADD},
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

int test_compiler_valDeclaration() {
    Compiler compiler;

    // Setup a source structure with a val declaration statement
    Source testSource = {
        .rootStatements = {&(Statement){
            .type = VAL_DECLARATION_STATEMENT,
            .as.valDeclarationStatement = &(ValDeclarationStatement){
                .identifier = &(IdentifierLiteral){
                    .token = (Token){.type = TOKEN_IDENTIFIER, .start = "x", .length = 1}},
                .expression = &(Expression){.type = PRIMARY_EXPRESSION, .as.primaryExpression = &(PrimaryExpression){.literal = &(Literal){.type = NUMBER_LITERAL, .as.numberLiteral = &(NumberLiteral){.token = (Token){.type = TOKEN_NUMBER, .start = "42", .length = 2}}}}}}}},
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

int test_compiler_variableDeclarationAndPrint() {
    // val x = 42;
    // print x;
    Source testSource = {
        .rootStatements = {
            &(Statement){
                .type = VAL_DECLARATION_STATEMENT,
                .as.valDeclarationStatement = &(ValDeclarationStatement){
                    .identifier = &(IdentifierLiteral){
                        .token = (Token){
                            .type = TOKEN_IDENTIFIER,
                            .start = "x",
                            .length = 1}},
                    .expression = &(Expression){.type = PRIMARY_EXPRESSION, .as.primaryExpression = &(PrimaryExpression){.literal = &(Literal){.type = NUMBER_LITERAL, .as.numberLiteral = &(NumberLiteral){.token = (Token){.type = TOKEN_NUMBER, .start = "42", .length = 2}}}}}}},
            &(Statement){.type = PRINT_STATEMENT, .as.printStatement = &(PrintStatement){.expression = &(Expression){.type = PRIMARY_EXPRESSION, .as.primaryExpression = &(PrimaryExpression){.literal = &(Literal){.type = IDENTIFIER_LITERAL, .as.identifierLiteral = &(IdentifierLiteral){.token = (Token){.type = TOKEN_IDENTIFIER, .start = "x", .length = 1}}}}}}}},
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