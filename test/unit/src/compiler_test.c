#include "compiler.h"

#include <string.h>

#include "../minunit.h"
#include "debug.h"
#include "syntax.h"

// -------------------------------------------------------------------------------
// ---------------------- Macros to facilitate creating AST ----------------------
// -------------------------------------------------------------------------------

#define TOKEN(typeArg) \
    (Token) { .type = typeArg, .start = "_", .length = 1 }

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

#define BOOLEAN_LITERAL(value)                              \
    &(Literal) {                                            \
        .type = BOOLEAN_LITERAL,                            \
        .as.booleanLiteral = &(BooleanLiteral) {            \
            .token = (Token) {                              \
                .type = (value) ? TOKEN_TRUE : TOKEN_FALSE, \
                .start = (value) ? "true" : "false",        \
                .length = (value) ? 4 : 5                   \
            }                                               \
        }                                                   \
    }

#define PRIMARY_EXPRESSION(literalInput)               \
    &(Expression) {                                    \
        .type = PRIMARY_EXPRESSION,                    \
        .as.primaryExpression = &(PrimaryExpression) { \
            .literal = literalInput                    \
        }                                              \
    }

#define ADDITIVE_EXPRESSION(left, punctuatorToken, right) \
    &(Expression) {                                       \
        .type = ADDITIVE_EXPRESSION,                      \
        .as.additiveExpression = &(AdditiveExpression) {  \
            .leftExpression = left,                       \
            .rightExpression = right,                     \
            .punctuator = punctuatorToken                 \
        }                                                 \
    }

// Macro to simplify the creation of a LOGICAL_OR_EXPRESSION
#define LOGICAL_OR_EXPRESSION(left, right)                 \
    &(Expression) {                                        \
        .type = LOGICAL_OR_EXPRESSION,                     \
        .as.logicalOrExpression = &(LogicalOrExpression) { \
            .leftExpression = left,                        \
            .rightExpression = right                       \
        }                                                  \
    }

// Macro to simplify the creation of a LOGICAL_AND_EXPRESSION
#define LOGICAL_AND_EXPRESSION(left, right)                  \
    &(Expression) {                                          \
        .type = LOGICAL_AND_EXPRESSION,                      \
        .as.logicalAndExpression = &(LogicalAndExpression) { \
            .leftExpression = left,                          \
            .rightExpression = right                         \
        }                                                    \
    }

// Macro to simplify the creation of an EQUALITY_EXPRESSION
#define EQUALITY_EXPRESSION(left, right, punctuatorToken) \
    &(Expression) {                                       \
        .type = EQUALITY_EXPRESSION,                      \
        .as.equalityExpression = &(EqualityExpression) {  \
            .leftExpression = left,                       \
            .rightExpression = right,                     \
            .punctuator = punctuatorToken                 \
        }                                                 \
    }

// Macro to simplify the creation of a COMPARISON_EXPRESSION
#define COMPARISON_EXPRESSION(left, right, punctuatorToken)  \
    &(Expression) {                                          \
        .type = COMPARISON_EXPRESSION,                       \
        .as.comparisonExpression = &(ComparisonExpression) { \
            .leftExpression = left,                          \
            .rightExpression = right,                        \
            .punctuator = punctuatorToken                    \
        }                                                    \
    }

// Macro to simplify the creation of an ADDITIVE_EXPRESSION
#define ADDITIVE_EXPRESSION(left, punctuatorToken, right) \
    &(Expression) {                                       \
        .type = ADDITIVE_EXPRESSION,                      \
        .as.additiveExpression = &(AdditiveExpression) {  \
            .leftExpression = left,                       \
            .rightExpression = right,                     \
            .punctuator = punctuatorToken                 \
        }                                                 \
    }

// Macro to simplify the creation of a MULTIPLICATIVE_EXPRESSION
#define MULTIPLICATIVE_EXPRESSION(left, punctuatorToken, right)      \
    &(Expression) {                                                  \
        .type = MULTIPLICATIVE_EXPRESSION,                           \
        .as.multiplicativeExpression = &(MultiplicativeExpression) { \
            .leftExpression = left,                                  \
            .rightExpression = right,                                \
            .punctuator = punctuatorToken                            \
        }                                                            \
    }

// Macro to simplify the creation of a UNARY_EXPRESSION
#define UNARY_EXPRESSION(punctuatorToken, right)   \
    &(Expression) {                                \
        .type = UNARY_EXPRESSION,                  \
        .as.unaryExpression = &(UnaryExpression) { \
            .punctuator = punctuatorToken,         \
            .rightExpression = right               \
        }                                          \
    }

// Macro to simplify the creation of a PRIMARY_EXPRESSION
#define PRIMARY_EXPRESSION(literalInput)               \
    &(Expression) {                                    \
        .type = PRIMARY_EXPRESSION,                    \
        .as.primaryExpression = &(PrimaryExpression) { \
            .literal = literalInput                    \
        }                                              \
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

#define UNARY_NEGATION_EXPRESSION(expression)      \
    &(Expression) {                                \
        .type = UNARY_EXPRESSION,                  \
        .as.unaryExpression = &(UnaryExpression) { \
            .punctuator = TOKEN(TOKEN_MINUS),      \
            .rightExpression = expression          \
        }                                          \
    }

#define UNARY_NOT_EXPRESSION(expression)            \
    &(Expression) {                                 \
        .type = UNARY_EXPRESSION,                   \
        .as.unaryExpression = &(UnaryExpression) {  \
            .punctuator = TOKEN(TOKEN_EXCLAMATION), \
            .rightExpression = expression           \
        }                                           \
    }

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
                    TOKEN(TOKEN_PLUS),
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

int test_compiler_binary_equal() {
    Compiler compiler;

    // Setup source with an equality expression
    Source testSource = {
        .rootStatements = {
            EXPRESSION_STATEMENT(
                EQUALITY_EXPRESSION(
                    PRIMARY_EXPRESSION(NUMBER_LITERAL("5")),
                    PRIMARY_EXPRESSION(NUMBER_LITERAL("5")),
                    TOKEN(TOKEN_EQUAL_EQUAL)))},
        .numberOfStatements = 1,
    };

    initCompiler(&compiler, &testSource);
    CompiledCode compiledCode = compile(&compiler);

    // Expected bytecode for '5 == 5'
    Bytecode expectedBytecode[] = {
        {.type = OP_LOAD_CONSTANT},  // Load 5
        {.type = OP_LOAD_CONSTANT},  // Load 5 again
        {.type = OP_BINARY_EQUAL},   // Compare equality
    };
    BytecodeArray expectedBytecodeArray = {.values = expectedBytecode, .used = 3};

    // Assert the actual bytecode matches the expected
    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.bytecodeArray));

    // Cleanup
    FREE_ARRAY(compiledCode.bytecodeArray);
    FREE_ARRAY(compiledCode.constantPool);

    return SUCCESS_RETURN_CODE;
}

int test_compiler_binary_not_equal() {
    Compiler compiler;

    // Setup source with a not equal expression
    Source testSource = {
        .rootStatements = {
            EXPRESSION_STATEMENT(
                EQUALITY_EXPRESSION(
                    PRIMARY_EXPRESSION(NUMBER_LITERAL("5")),
                    PRIMARY_EXPRESSION(NUMBER_LITERAL("7")),
                    TOKEN(TOKEN_EXCLAMATION_EQUAL)))},
        .numberOfStatements = 1,
    };

    initCompiler(&compiler, &testSource);
    CompiledCode compiledCode = compile(&compiler);

    // Expected bytecode for '5 != 7'
    Bytecode expectedBytecode[] = {
        {.type = OP_LOAD_CONSTANT},     // Load 5
        {.type = OP_LOAD_CONSTANT},     // Load 7
        {.type = OP_BINARY_NOT_EQUAL},  // Check not equal
    };
    BytecodeArray expectedBytecodeArray = {.values = expectedBytecode, .used = 3};

    // Assert the bytecode is as expected
    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.bytecodeArray));

    // Cleanup
    FREE_ARRAY(compiledCode.bytecodeArray);
    FREE_ARRAY(compiledCode.constantPool);

    return SUCCESS_RETURN_CODE;
}

int test_compiler_comparison_operations() {
    Compiler compiler;

    // Setup source with multiple comparison expressions
    Source testSource = {
        .rootStatements = {
            // 5 > 3
            EXPRESSION_STATEMENT(
                COMPARISON_EXPRESSION(
                    PRIMARY_EXPRESSION(NUMBER_LITERAL("5")),
                    PRIMARY_EXPRESSION(NUMBER_LITERAL("3")),
                    TOKEN(TOKEN_GREATER))),
            // 5 >= 5
            EXPRESSION_STATEMENT(
                COMPARISON_EXPRESSION(
                    PRIMARY_EXPRESSION(NUMBER_LITERAL("5")),
                    PRIMARY_EXPRESSION(NUMBER_LITERAL("5")),
                    TOKEN(TOKEN_GREATER_EQUAL))),
            // 3 < 5
            EXPRESSION_STATEMENT(
                COMPARISON_EXPRESSION(
                    PRIMARY_EXPRESSION(NUMBER_LITERAL("3")),
                    PRIMARY_EXPRESSION(NUMBER_LITERAL("5")),
                    TOKEN(TOKEN_LESSER))),
            // 5 <= 5
            EXPRESSION_STATEMENT(
                COMPARISON_EXPRESSION(
                    PRIMARY_EXPRESSION(NUMBER_LITERAL("5")),
                    PRIMARY_EXPRESSION(NUMBER_LITERAL("5")),
                    TOKEN(TOKEN_LESSER_EQUAL)))},
        .numberOfStatements = 4,
    };

    initCompiler(&compiler, &testSource);
    CompiledCode compiledCode = compile(&compiler);

    // Expected bytecode for the comparison expressions
    Bytecode expectedBytecode[] = {
        // 5 > 3
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_BINARY_GT},
        // 5 >= 5
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_BINARY_GTE},
        // 3 < 5
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_BINARY_LT},
        // 5 <= 5
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_BINARY_LTE},
    };
    BytecodeArray expectedBytecodeArray = {.values = expectedBytecode, .used = sizeof(expectedBytecode) / sizeof(Bytecode)};

    // Assert the actual bytecode matches the expected
    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.bytecodeArray));

    // Cleanup
    FREE_ARRAY(compiledCode.bytecodeArray);
    FREE_ARRAY(compiledCode.constantPool);

    return SUCCESS_RETURN_CODE;
}

int test_compiler_logical_and_or_operations() {
    Compiler compiler;

    // Setup source with logical AND and OR expressions
    Source testSource = {
        .rootStatements = {
            // true && false
            EXPRESSION_STATEMENT(
                LOGICAL_AND_EXPRESSION(
                    PRIMARY_EXPRESSION(BOOLEAN_LITERAL(true)),
                    PRIMARY_EXPRESSION(BOOLEAN_LITERAL(false)))),
            // true || false
            EXPRESSION_STATEMENT(
                LOGICAL_OR_EXPRESSION(
                    PRIMARY_EXPRESSION(BOOLEAN_LITERAL(true)),
                    PRIMARY_EXPRESSION(BOOLEAN_LITERAL(false)))),
        },
        .numberOfStatements = 2,
    };

    initCompiler(&compiler, &testSource);
    CompiledCode compiledCode = compile(&compiler);

    // Expected bytecode for the logical AND and OR expressions
    Bytecode expectedBytecode[] = {
        // true && false
        {.type = OP_TRUE},
        {.type = OP_FALSE},
        {.type = OP_BINARY_LOGICAL_AND},
        // true || false
        {.type = OP_TRUE},
        {.type = OP_FALSE},
        {.type = OP_BINARY_LOGICAL_OR},
    };
    BytecodeArray expectedBytecodeArray = {.values = expectedBytecode, .used = sizeof(expectedBytecode) / sizeof(Bytecode)};

    // Assert the actual bytecode matches the expected
    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.bytecodeArray));

    // Cleanup
    FREE_ARRAY(compiledCode.bytecodeArray);
    FREE_ARRAY(compiledCode.constantPool);

    return SUCCESS_RETURN_CODE;
}

int test_compiler_multiplicative_expressions() {
    Compiler compiler;

    // Setup source with multiplicative expressions
    Source testSource = {
        .rootStatements = {
            // 5 * 2
            EXPRESSION_STATEMENT(
                MULTIPLICATIVE_EXPRESSION(
                    PRIMARY_EXPRESSION(NUMBER_LITERAL("5")),
                    TOKEN(TOKEN_STAR),
                    PRIMARY_EXPRESSION(NUMBER_LITERAL("2")))),
            // 10 / 2
            EXPRESSION_STATEMENT(
                MULTIPLICATIVE_EXPRESSION(
                    PRIMARY_EXPRESSION(NUMBER_LITERAL("10")),
                    TOKEN(TOKEN_SLASH),
                    PRIMARY_EXPRESSION(NUMBER_LITERAL("2"))))},
        .numberOfStatements = 2,
    };

    initCompiler(&compiler, &testSource);
    CompiledCode compiledCode = compile(&compiler);

    // Expected bytecode for the multiplicative expressions
    Bytecode expectedBytecode[] = {
        // 5 * 2
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_BINARY_MULTIPLY},
        // 10 / 2
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_BINARY_DIVIDE},
    };
    BytecodeArray expectedBytecodeArray = {.values = expectedBytecode, .used = sizeof(expectedBytecode) / sizeof(Bytecode)};

    // Assert the actual bytecode matches the expected
    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.bytecodeArray));

    // Verify constants in the constant pool
    ASSERT(compiledCode.constantPool.values[0].as.number == 5);
    ASSERT(compiledCode.constantPool.values[1].as.number == 2);
    ASSERT(compiledCode.constantPool.values[2].as.number == 10);

    // Cleanup
    FREE_ARRAY(compiledCode.bytecodeArray);
    FREE_ARRAY(compiledCode.constantPool);

    return SUCCESS_RETURN_CODE;
}

int test_compiler_unary_expressions() {
    Compiler compiler;

    // Construct an AST for the expressions: -42 and !true
    Source testSource = {
        .rootStatements = {
            EXPRESSION_STATEMENT(
                UNARY_NEGATION_EXPRESSION(
                    PRIMARY_EXPRESSION(NUMBER_LITERAL("42")))),
            EXPRESSION_STATEMENT(
                UNARY_NOT_EXPRESSION(
                    PRIMARY_EXPRESSION(BOOLEAN_LITERAL(true))))},
        .numberOfStatements = 2,
    };

    initCompiler(&compiler, &testSource);
    CompiledCode compiledCode = compile(&compiler);

    // Expected bytecode: Load constant 42, apply unary negation, push true, apply logical NOT
    Bytecode expectedBytecode[] = {
        {.type = OP_LOAD_CONSTANT},  // Load 42
        {.type = OP_UNARY_NEGATE},   // Apply unary negation
        {.type = OP_TRUE},           // Push true
        {.type = OP_UNARY_NOT},      // Apply logical NOT
    };
    BytecodeArray expectedBytecodeArray = {.values = expectedBytecode, .used = sizeof(expectedBytecode) / sizeof(Bytecode)};

    // Compare actual and expected bytecode
    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.bytecodeArray));

    // Clean up
    FREE_ARRAY(compiledCode.bytecodeArray);
    FREE_ARRAY(compiledCode.constantPool);

    return SUCCESS_RETURN_CODE;
}

int test_compiler_boolean_literal() {
    Compiler compiler;

    Source testSource = (Source){
        .rootStatements = {
            EXPRESSION_STATEMENT(PRIMARY_EXPRESSION(BOOLEAN_LITERAL(false)))},
        .numberOfStatements = 1,
    };

    initCompiler(&compiler, &testSource);
    CompiledCode compiledCodeFalse = compile(&compiler);

    ASSERT(compiledCodeFalse.bytecodeArray.used == 1);
    ASSERT(compiledCodeFalse.bytecodeArray.values[0].type == OP_FALSE);

    FREE_ARRAY(compiledCodeFalse.bytecodeArray);
    FREE_ARRAY(compiledCodeFalse.constantPool);

    return SUCCESS_RETURN_CODE;
}