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

#define STRING_LITERAL(str)                                                                \
    &(Literal) {                                                                           \
        .type = STRING_LITERAL,                                                            \
        .as.stringLiteral = &(StringLiteral) {                                             \
            .token = (Token) { .type = TOKEN_STRING, .start = str, .length = strlen(str) } \
        }                                                                                  \
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

#define BLOCK_STATEMENT(...)                                                      \
    &(Statement) {                                                                \
        .type = BLOCK_STATEMENT,                                                  \
        .as.blockStatement = &(BlockStatement) {                                  \
            .statementArray = (StatementArray) {                                  \
                .values = (Statement*[]){__VA_ARGS__},                            \
                .used = sizeof((Statement*[]){__VA_ARGS__}) / sizeof(Statement*), \
                .size = sizeof((Statement*[]){__VA_ARGS__}) / sizeof(Statement*)  \
            }                                                                     \
        }                                                                         \
    }

#define SELECTION_STATEMENT(conditionExpr, trueStatementArg, falseStatementArg) \
    &(Statement) {                                                              \
        .type = SELECTION_STATEMENT,                                            \
        .as.selectionStatement = &(SelectionStatement) {                        \
            .conditionExpression = conditionExpr,                               \
            .trueStatement = trueStatementArg,                                  \
            .falseStatement = falseStatementArg                                 \
        }                                                                       \
    }

#define BLOCK_EXPRESSION(stmts, lastExpr)                                    \
    &(Expression) {                                                          \
        .type = BLOCK_EXPRESSION,                                            \
        .as.blockExpression = &(BlockExpression) {                           \
            .statementArray = (StatementArray){                              \
                .values = (Statement*[]){stmts},                             \
                .used = sizeof((Statement*[]){stmts}) / sizeof(Statement*),  \
                .size = sizeof((Statement*[]){stmts}) / sizeof(Statement*)}, \
            .lastExpression = lastExpr                                       \
        }                                                                    \
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

// Macro to initialize compiler, compile AST, and print the compiled bytecode
// Assumes the source is named `testSource`; creates a variable called `compiledCode`.
#define COMPILE_TEST_SOURCE                         \
    Compiler compiler;                              \
    initCompiler(&compiler, &testSource);           \
    CompiledCode compiledCode = compile(&compiler); \
    printCompiledCode(compiledCode);

// -------------------------------------------------------------------------
// --------------------------------- Tests ---------------------------------
// -------------------------------------------------------------------------

// Test basic arithmetic expression
int test_compiler() {
    Source testSource = {
        .rootStatements = {
            EXPRESSION_STATEMENT(
                ADDITIVE_EXPRESSION(
                    PRIMARY_EXPRESSION(NUMBER_LITERAL("5")),
                    TOKEN(TOKEN_PLUS),
                    PRIMARY_EXPRESSION(NUMBER_LITERAL("7"))))},
        .numberOfStatements = 1,
    };

    COMPILE_TEST_SOURCE

    // Expected bytecode
    Bytecode expectedBytecode[] = {
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_BINARY_ADD},
        {.type = OP_POPN},
    };
    BytecodeArray expectedBytecodeArray = {.values = expectedBytecode, .used = 4};

    // Compared the actual and expected
    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.bytecodeArray));
    ASSERT(compiledCode.constantPool.values[0].as.number == 5);       // 5 is in slot 0 of the pool
    ASSERT(compiledCode.bytecodeArray.values[0].maybeOperand1 == 0);  // first instruction loads slot 0
    ASSERT(compiledCode.constantPool.values[1].as.number == 7);
    ASSERT(compiledCode.bytecodeArray.values[1].maybeOperand1 == 1);

    // Clean up
    FREE_ARRAY(compiledCode.bytecodeArray);
    FREE_ARRAY(compiledCode.constantPool);

    return SUCCESS_RETURN_CODE;
}

int test_compiler_print() {
    // Set up a source structure with a print statement
    Source testSource = {
        .rootStatements = {
            PRINT_STATEMENT(PRIMARY_EXPRESSION(NUMBER_LITERAL("42")))},
        .numberOfStatements = 1,
    };

    COMPILE_TEST_SOURCE

    // Verify the bytecode
    // Assuming the bytecode for a print statement is OP_PRINT followed by the value to print
    ASSERT(compiledCode.bytecodeArray.used == 2);                           // Check if two bytecode instructions are generated
    ASSERT(compiledCode.bytecodeArray.values[0].type == OP_LOAD_CONSTANT);  // First should be OP_CONSTANT
    ASSERT(compiledCode.bytecodeArray.values[1].type == OP_PRINT);          // Second should be OP_PRINT

    // Clean up
    FREE_ARRAY(compiledCode.bytecodeArray);
    FREE_ARRAY(compiledCode.constantPool);

    return SUCCESS_RETURN_CODE;
}

int test_compiler_val_declaration() {
    // Setup a source structure with a val declaration statement
    Source testSource = {
        .rootStatements = {
            VAL_DECLARATION_STATEMENT("x", PRIMARY_EXPRESSION(NUMBER_LITERAL("42")))},
        .numberOfStatements = 1,
    };

    COMPILE_TEST_SOURCE

    // Verify bytecode for val declaration
    ASSERT(compiledCode.bytecodeArray.used == 2);                               // Check if two bytecode instructions are generated
    ASSERT(compiledCode.bytecodeArray.values[0].type == OP_LOAD_CONSTANT);      // First should be OP_LOAD_CONSTANT
    ASSERT(compiledCode.bytecodeArray.values[1].type == OP_DEFINE_GLOBAL_VAL);  // Second should be OP_SET_GLOBAL_VAL

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

    COMPILE_TEST_SOURCE

    ASSERT(compiledCode.bytecodeArray.values[0].type == OP_LOAD_CONSTANT);      // load 42 into stack
    ASSERT(compiledCode.bytecodeArray.values[1].type == OP_DEFINE_GLOBAL_VAL);  // assign top of stack to variable 'x'
    ASSERT(compiledCode.bytecodeArray.values[2].type == OP_GET_GLOBAL_VAL);     // get variable 'x'
    ASSERT(compiledCode.bytecodeArray.values[3].type == OP_PRINT);              // print top of stack

    ASSERT(compiledCode.constantPool.values[0].type == CONST_TYPE_DOUBLE);  // 42
    ASSERT(compiledCode.constantPool.values[0].as.number == 42);
    ASSERT(compiledCode.constantPool.values[1].type == CONST_TYPE_IDENTIFIER);  // x

    FREE_ARRAY(compiledCode.bytecodeArray);
    FREE_ARRAY(compiledCode.constantPool);

    return SUCCESS_RETURN_CODE;  // Assuming SUCCESS_RETURN_CODE is defined as part of your testing framework
}

int test_add_constant_to_pool_no_duplicates() {
    Source testSource = (Source){
        .rootStatements = {
            VAL_DECLARATION_STATEMENT("x", PRIMARY_EXPRESSION(NUMBER_LITERAL("42"))),
            PRINT_STATEMENT(PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("x"))),
            VAL_DECLARATION_STATEMENT("y", PRIMARY_EXPRESSION(NUMBER_LITERAL("42")))},
        .numberOfStatements = 3,
    };

    COMPILE_TEST_SOURCE

    // Assert the constants were not added twice; there should be one for '42', one for 'x', one for 'y'.
    ASSERT(compiledCode.constantPool.used == 3);

    // Cleanup
    FREE_ARRAY(compiler.compiledBytecode);
    FREE_ARRAY(compiler.constantPool);

    return SUCCESS_RETURN_CODE;
}

int test_compiler_binary_equal() {
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

    COMPILE_TEST_SOURCE

    // Expected bytecode for '5 == 5'
    Bytecode expectedBytecode[] = {
        {.type = OP_LOAD_CONSTANT},  // Load 5
        {.type = OP_LOAD_CONSTANT},  // Load 5 again
        {.type = OP_BINARY_EQUAL},   // Compare equality
        {.type = OP_POPN},           // Expression statements pop the value left on the stack
    };
    BytecodeArray expectedBytecodeArray = {.values = expectedBytecode, .used = 4};

    // Assert the actual bytecode matches the expected
    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.bytecodeArray));

    // Cleanup
    FREE_ARRAY(compiledCode.bytecodeArray);
    FREE_ARRAY(compiledCode.constantPool);

    return SUCCESS_RETURN_CODE;
}

int test_compiler_binary_not_equal() {
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

    COMPILE_TEST_SOURCE

    // Expected bytecode for '5 != 7'
    Bytecode expectedBytecode[] = {
        {.type = OP_LOAD_CONSTANT},     // Load 5
        {.type = OP_LOAD_CONSTANT},     // Load 7
        {.type = OP_BINARY_NOT_EQUAL},  // Check not equal
        {.type = OP_POPN},
    };
    BytecodeArray expectedBytecodeArray = {.values = expectedBytecode, .used = 4};

    // Assert the bytecode is as expected
    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.bytecodeArray));

    // Cleanup
    FREE_ARRAY(compiledCode.bytecodeArray);
    FREE_ARRAY(compiledCode.constantPool);

    return SUCCESS_RETURN_CODE;
}

int test_compiler_comparison_operations() {
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

    COMPILE_TEST_SOURCE

    // Expected bytecode for the comparison expressions
    Bytecode expectedBytecode[] = {
        // 5 > 3
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_BINARY_GT},
        {.type = OP_POPN},
        // 5 >= 5
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_BINARY_GTE},
        {.type = OP_POPN},
        // 3 < 5
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_BINARY_LT},
        {.type = OP_POPN},
        // 5 <= 5
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_BINARY_LTE},
        {.type = OP_POPN},
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

    COMPILE_TEST_SOURCE

    // Expected bytecode for the logical AND and OR expressions
    Bytecode expectedBytecode[] = {
        // true && false
        {.type = OP_TRUE},
        {.type = OP_FALSE},
        {.type = OP_BINARY_LOGICAL_AND},
        {.type = OP_POPN},
        // true || false
        {.type = OP_TRUE},
        {.type = OP_FALSE},
        {.type = OP_BINARY_LOGICAL_OR},
        {.type = OP_POPN},
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

    COMPILE_TEST_SOURCE

    // Expected bytecode for the multiplicative expressions
    Bytecode expectedBytecode[] = {
        // 5 * 2
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_BINARY_MULTIPLY},
        {.type = OP_POPN},
        // 10 / 2
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_BINARY_DIVIDE},
        {.type = OP_POPN},
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

    COMPILE_TEST_SOURCE

    // Expected bytecode: Load constant 42, apply unary negation, push true, apply logical NOT
    Bytecode expectedBytecode[] = {
        {.type = OP_LOAD_CONSTANT},  // Load 42
        {.type = OP_UNARY_NEGATE},   // Apply unary negation
        {.type = OP_POPN},
        {.type = OP_TRUE},       // Push true
        {.type = OP_UNARY_NOT},  // Apply logical NOT
        {.type = OP_POPN},
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
    Source testSource = (Source){
        .rootStatements = {
            EXPRESSION_STATEMENT(PRIMARY_EXPRESSION(BOOLEAN_LITERAL(false)))},
        .numberOfStatements = 1,
    };

    COMPILE_TEST_SOURCE

    ASSERT(compiledCode.bytecodeArray.used == 2);
    ASSERT(compiledCode.bytecodeArray.values[0].type == OP_FALSE);

    FREE_ARRAY(compiledCode.bytecodeArray);
    FREE_ARRAY(compiledCode.constantPool);

    return SUCCESS_RETURN_CODE;
}

int test_compiler_string_literal() {
    // Define a source with a string literal
    Source testSource = {
        .rootStatements = {
            EXPRESSION_STATEMENT(
                PRIMARY_EXPRESSION(STRING_LITERAL("\"Hello, World!\"")))},
        .numberOfStatements = 1,
    };

    // Initialize and compile
    COMPILE_TEST_SOURCE

    // Assertions to ensure string literal was correctly recognized, added to the constant pool, and properly loaded
    ASSERT(compiledCode.constantPool.used == 1);                                          // Ensure constant pool has entries
    ASSERT(strcmp(compiledCode.constantPool.values[0].as.string, "Hello, World!") == 0);  // Check string content
    ASSERT(compiledCode.bytecodeArray.values[0].type == OP_LOAD_CONSTANT);                // Check bytecode to load string

    // Cleanup
    FREE_ARRAY(compiledCode.bytecodeArray);
    FREE_ARRAY(compiledCode.constantPool);

    return SUCCESS_RETURN_CODE;
}

int test_compiler_stack_height_expression_and_val() {
    // Setup a source with an expression statement followed by a val declaration
    Source testSource = {
        .rootStatements = {
            EXPRESSION_STATEMENT(
                PRIMARY_EXPRESSION(NUMBER_LITERAL("42"))),
            PRINT_STATEMENT(PRIMARY_EXPRESSION(NUMBER_LITERAL("42"))),
            VAL_DECLARATION_STATEMENT("x", PRIMARY_EXPRESSION(NUMBER_LITERAL("42")))},
        .numberOfStatements = 3,
    };

    COMPILE_TEST_SOURCE

    // The expression and print statements shouldn't add to the height.
    // The val declaration also shouldn't add anything because it's a global variable.
    ASSERT(compiler.currentStackHeight == 0);

    // Clean up
    FREE_ARRAY(compiledCode.bytecodeArray);
    FREE_ARRAY(compiledCode.constantPool);

    return SUCCESS_RETURN_CODE;
}

// Test block statements with local variables
int test_compiler_single_block_statement_with_locals() {
    // Set up a source structure with a block statement containing local variable declarations and usage
    Source testSource = {
        .rootStatements = {
            BLOCK_STATEMENT(VAL_DECLARATION_STATEMENT("x", PRIMARY_EXPRESSION(NUMBER_LITERAL("10"))),
                            PRINT_STATEMENT(PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("x"))),
                            VAL_DECLARATION_STATEMENT("y", PRIMARY_EXPRESSION(NUMBER_LITERAL("20"))),
                            PRINT_STATEMENT(PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("y"))))},
        .numberOfStatements = 1,
    };

    COMPILE_TEST_SOURCE

    // Define expected bytecode
    Bytecode expectedBytecode[] = {
        {.type = OP_LOAD_CONSTANT},          // Load 10
        {.type = OP_DEFINE_LOCAL_VAL_FAST},  // Set local 'x'
        {.type = OP_GET_LOCAL_VAL_FAST},     // Get local 'x'
        {.type = OP_PRINT},                  // Print 'x'
        {.type = OP_LOAD_CONSTANT},          // Load 20
        {.type = OP_DEFINE_LOCAL_VAL_FAST},  // Set local 'y'
        {.type = OP_GET_LOCAL_VAL_FAST},     // Get local 'y'
        {.type = OP_PRINT},                  // Print 'y'
        {.type = OP_POPN},                   // Cleanup 'x' and 'y'
    };
    BytecodeArray expectedBytecodeArray = {.values = expectedBytecode, .used = sizeof(expectedBytecode) / sizeof(Bytecode)};

    // Assert the actual bytecode matches the expected
    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.bytecodeArray));

    // Cleanup
    FREE_ARRAY(compiledCode.bytecodeArray);
    FREE_ARRAY(compiledCode.constantPool);

    return SUCCESS_RETURN_CODE;
}

int test_compiler_nested_blocks_with_global_and_local_vars() {
    /*
    val g = 100;
    print g;
    {
        print g;
        val x = 10;
        print x;
        {
            val g = 20;
            print g;
            val y = 30;
            print y;
        }
    }
    */
    Source testSource = {
        .rootStatements = {
            VAL_DECLARATION_STATEMENT("g", PRIMARY_EXPRESSION(NUMBER_LITERAL("100"))),
            PRINT_STATEMENT(PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("g"))),
            BLOCK_STATEMENT(
                PRINT_STATEMENT(PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("g"))),
                VAL_DECLARATION_STATEMENT("x", PRIMARY_EXPRESSION(NUMBER_LITERAL("10"))),
                PRINT_STATEMENT(PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("x"))),
                BLOCK_STATEMENT(
                    VAL_DECLARATION_STATEMENT("g", PRIMARY_EXPRESSION(NUMBER_LITERAL("20"))),
                    PRINT_STATEMENT(PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("g"))),
                    VAL_DECLARATION_STATEMENT("y", PRIMARY_EXPRESSION(NUMBER_LITERAL("30"))),
                    PRINT_STATEMENT(PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("y")))))},
        .numberOfStatements = 3,
    };

    COMPILE_TEST_SOURCE

    Bytecode expectedBytecode[] = {
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_DEFINE_GLOBAL_VAL},
        {.type = OP_GET_GLOBAL_VAL},
        {.type = OP_PRINT},
        {.type = OP_GET_GLOBAL_VAL},  // Global access inside local scope
        {.type = OP_PRINT},
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_DEFINE_LOCAL_VAL_FAST},
        {.type = OP_GET_LOCAL_VAL_FAST},  // No constant pool access; the VM won't even know the name of the val.
        {.type = OP_PRINT},
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_DEFINE_LOCAL_VAL_FAST},
        {.type = OP_GET_LOCAL_VAL_FAST},
        {.type = OP_PRINT},
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_DEFINE_LOCAL_VAL_FAST},
        {.type = OP_GET_LOCAL_VAL_FAST},
        {.type = OP_PRINT},
        {.type = OP_POPN},
        {.type = OP_POPN},
    };

    BytecodeArray expectedBytecodeArray = {.values = expectedBytecode, .used = sizeof(expectedBytecode) / sizeof(Bytecode)};

    // Assert the actual bytecode matches the expected
    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.bytecodeArray));

    // Assert constant pool contains one identifier for the global, and four numbers.
    //  Constant Pool:
    //  #0 (double) 100.000000
    //  #1 (identifier) "g"
    //  #2 (double) 10.000000
    //  #3 (double) 20.000000
    //  #4 (double) 30.000000
    ASSERT(compiledCode.constantPool.used == 5);
    ASSERT(compiledCode.constantPool.values[0].type == CONST_TYPE_DOUBLE);
    ASSERT(compiledCode.constantPool.values[0].as.number == 100.0);
    ASSERT(compiledCode.constantPool.values[1].type == CONST_TYPE_IDENTIFIER);
    ASSERT(strcmp(compiledCode.constantPool.values[1].as.string, "g") == 0);
    ASSERT(compiledCode.constantPool.values[2].type == CONST_TYPE_DOUBLE);
    ASSERT(compiledCode.constantPool.values[2].as.number == 10.0);
    ASSERT(compiledCode.constantPool.values[3].type == CONST_TYPE_DOUBLE);
    ASSERT(compiledCode.constantPool.values[3].as.number == 20.0);
    ASSERT(compiledCode.constantPool.values[4].type == CONST_TYPE_DOUBLE);
    ASSERT(compiledCode.constantPool.values[4].as.number == 30.0);

    // Cleanup and assertions
    FREE_ARRAY(compiledCode.bytecodeArray);
    FREE_ARRAY(compiledCode.constantPool);

    return SUCCESS_RETURN_CODE;
}

int test_compiler_if_statement_no_else() {
    // if (true) { print("Hello, World!") };
    Source testSource = {
        .rootStatements = {
            SELECTION_STATEMENT(
                PRIMARY_EXPRESSION(BOOLEAN_LITERAL(true)),
                BLOCK_STATEMENT(PRINT_STATEMENT(PRIMARY_EXPRESSION(STRING_LITERAL("\"Hello, World!\"")))),
                NULL)},
        .numberOfStatements = 1,
    };

    COMPILE_TEST_SOURCE

    Bytecode expectedBytecode[] = {
        {.type = OP_TRUE},
        {.type = OP_JUMP_IF_FALSE},
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_PRINT},
        {.type = OP_POPN, .maybeOperand1 = 0},
    };
    BytecodeArray expectedBytecodeArray = {.values = expectedBytecode, .used = sizeof(expectedBytecode) / sizeof(Bytecode)};

    // Assert the actual bytecode matches the expected
    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.bytecodeArray));

    // Check that nothing is popped (we didnt create any variables)
    ASSERT(expectedBytecode[4].maybeOperand1 == compiledCode.bytecodeArray.values[4].maybeOperand1);

    // Cleanup
    FREE_ARRAY(compiledCode.bytecodeArray);
    FREE_ARRAY(compiledCode.constantPool);

    return SUCCESS_RETURN_CODE;
}

int test_compiler_if_statement_with_else() {
    // if (false) { print("Hello, World!"); } else { print("Goodbye, World!"); };
    Source testSource = {
        .rootStatements = {
            SELECTION_STATEMENT(
                PRIMARY_EXPRESSION(BOOLEAN_LITERAL(false)),
                BLOCK_STATEMENT(PRINT_STATEMENT(PRIMARY_EXPRESSION(STRING_LITERAL("\"Hello, World!\"")))),
                BLOCK_STATEMENT(PRINT_STATEMENT(PRIMARY_EXPRESSION(STRING_LITERAL("\"Goodbye, World!\"")))))},
        .numberOfStatements = 1,
    };

    COMPILE_TEST_SOURCE

    Bytecode expectedBytecode[] = {
        {.type = OP_FALSE},                     // Condition evaluation
        {.type = OP_JUMP_IF_FALSE},             // Jump if condition is false
        {.type = OP_LOAD_CONSTANT},             // Load "Hello, World!"
        {.type = OP_PRINT},                     // Print
        {.type = OP_POPN, .maybeOperand1 = 0},  // Clean up "then" block
        {.type = OP_JUMP},                      // Jump to end to skip else branch
        {.type = OP_LOAD_CONSTANT},             // Load "Goodbye, World!" for else
        {.type = OP_PRINT},                     // Print for else
        {.type = OP_POPN, .maybeOperand1 = 0}   // Clean up "then" block,
    };
    BytecodeArray expectedBytecodeArray = {.values = expectedBytecode, .used = sizeof(expectedBytecode) / sizeof(Bytecode)};

    // Assert the actual bytecode matches the expected
    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.bytecodeArray));

    // Check that nothing is popped (we didnt create any variables)
    ASSERT(expectedBytecode[4].maybeOperand1 == compiledCode.bytecodeArray.values[4].maybeOperand1);
    ASSERT(expectedBytecode[8].maybeOperand1 == compiledCode.bytecodeArray.values[8].maybeOperand1);

    // Cleanup
    FREE_ARRAY(compiledCode.bytecodeArray);
    FREE_ARRAY(compiledCode.constantPool);

    return SUCCESS_RETURN_CODE;
}

int test_compiler_nested_if_statements() {
    /*
    if (true) {
        print "true-outer";
        if (true) {
            print "true-inner";
        } else {
            print "false-inner";
        }
    } else {
        print "false-outer";
    }
    */
    Source testSource = {
        .rootStatements = {
            SELECTION_STATEMENT(
                PRIMARY_EXPRESSION(BOOLEAN_LITERAL(true)),  // Outer if condition
                BLOCK_STATEMENT(                            // True branch of the outer if
                    PRINT_STATEMENT(PRIMARY_EXPRESSION(STRING_LITERAL("\"true-outer\""))),
                    SELECTION_STATEMENT(
                        PRIMARY_EXPRESSION(BOOLEAN_LITERAL(true)),                                               // Inner if condition
                        BLOCK_STATEMENT(PRINT_STATEMENT(PRIMARY_EXPRESSION(STRING_LITERAL("\"true-inner\"")))),  // True branch of the inner if
                        BLOCK_STATEMENT(PRINT_STATEMENT(PRIMARY_EXPRESSION(STRING_LITERAL("\"false-inner\""))))  // False branch of the inner if
                        )),
                BLOCK_STATEMENT(PRINT_STATEMENT(PRIMARY_EXPRESSION(STRING_LITERAL("\"false-outer\""))))  // False branch of the outer if
                )},
        .numberOfStatements = 1,
    };

    COMPILE_TEST_SOURCE

    // Expected bytecode and constant pool for the nested if statements
    Bytecode expectedBytecode[] = {
        {.type = OP_TRUE},
        {.type = OP_JUMP_IF_FALSE, .maybeOperand1 = 15},
        {.type = OP_LOAD_CONSTANT, .maybeOperand1 = 0},
        {.type = OP_PRINT},
        {.type = OP_TRUE},
        {.type = OP_JUMP_IF_FALSE, .maybeOperand1 = 10},
        {.type = OP_LOAD_CONSTANT, .maybeOperand1 = 1},
        {.type = OP_PRINT},
        {.type = OP_POPN, .maybeOperand1 = 0},
        {.type = OP_JUMP, .maybeOperand1 = 13},
        {.type = OP_LOAD_CONSTANT, .maybeOperand1 = 2},
        {.type = OP_PRINT},
        {.type = OP_POPN, .maybeOperand1 = 0},
        {.type = OP_POPN, .maybeOperand1 = 0},
        {.type = OP_JUMP, .maybeOperand1 = 18},
        {.type = OP_LOAD_CONSTANT, .maybeOperand1 = 3},
        {.type = OP_PRINT},
        {.type = OP_POPN, .maybeOperand1 = 0},
    };
    BytecodeArray expectedBytecodeArray = {.values = expectedBytecode, .used = sizeof(expectedBytecode) / sizeof(Bytecode)};

    // Assert bytecode matches expected
    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.bytecodeArray));

    // Verify operands
    ASSERT(compiledCode.bytecodeArray.values[1].maybeOperand1 == 15);   // Operand for first OP_JUMP_IF_FALSE
    ASSERT(compiledCode.bytecodeArray.values[5].maybeOperand1 == 10);   // Operand for nested OP_JUMP_IF_FALSE
    ASSERT(compiledCode.bytecodeArray.values[9].maybeOperand1 == 13);   // Operand for first OP_JUMP
    ASSERT(compiledCode.bytecodeArray.values[14].maybeOperand1 == 18);  // Operand for second OP_JUMP
    ASSERT(compiledCode.bytecodeArray.values[8].maybeOperand1 == 0);    // Operand for the first OP_POPN after "true-inner"
    ASSERT(compiledCode.bytecodeArray.values[12].maybeOperand1 == 0);   // Operand for the OP_POPN after "false-inner"
    ASSERT(compiledCode.bytecodeArray.values[13].maybeOperand1 == 0);   // Operand for the OP_POPN after exiting the inner if
    ASSERT(compiledCode.bytecodeArray.values[17].maybeOperand1 == 0);   // Operand for the OP_POPN after "false-outer"

    // Clean up
    FREE_ARRAY(compiledCode.bytecodeArray);
    FREE_ARRAY(compiledCode.constantPool);

    return SUCCESS_RETURN_CODE;
}

int test_compiler_block_expression_simple() {
    // val a = { 3; };
    Source testSource = {
        .rootStatements = {
            VAL_DECLARATION_STATEMENT(
                "a",
                BLOCK_EXPRESSION(
                    ,
                    PRIMARY_EXPRESSION(NUMBER_LITERAL("3"))))},
        .numberOfStatements = 1,
    };

    COMPILE_TEST_SOURCE

    Bytecode expectedBytecode[] = {
        {.type = OP_LOAD_CONSTANT, .maybeOperand1 = 0},
        {.type = OP_SWAP, .maybeOperand1 = 0},
        {.type = OP_POPN, .maybeOperand1 = 0},
        {.type = OP_DEFINE_GLOBAL_VAL, .maybeOperand1 = 1},
    };
    BytecodeArray expectedBytecodeArray = {.values = expectedBytecode, .used = 4};

    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.bytecodeArray));
    ASSERT(compiledCode.constantPool.values[0].as.number == 3);
    ASSERT(strcmp(compiledCode.constantPool.values[1].as.string, "a") == 0);

    FREE_ARRAY(compiledCode.bytecodeArray);
    FREE_ARRAY(compiledCode.constantPool);

    return SUCCESS_RETURN_CODE;
}

int test_compiler_block_expression_nested() {
    // val a = {{{ 3; };}}
    Source testSource = {
        .rootStatements = {
            VAL_DECLARATION_STATEMENT(
                "a",
                BLOCK_EXPRESSION(
                    ,
                    BLOCK_EXPRESSION(
                        ,
                        BLOCK_EXPRESSION(
                            ,
                            PRIMARY_EXPRESSION(NUMBER_LITERAL("3"))))))},
        .numberOfStatements = 1,
    };

    COMPILE_TEST_SOURCE

    Bytecode expectedBytecode[] = {
        {.type = OP_LOAD_CONSTANT, .maybeOperand1 = 0},
        {.type = OP_SWAP, .maybeOperand1 = 0},
        {.type = OP_POPN, .maybeOperand1 = 0},
        {.type = OP_SWAP, .maybeOperand1 = 0},
        {.type = OP_POPN, .maybeOperand1 = 0},
        {.type = OP_SWAP, .maybeOperand1 = 0},
        {.type = OP_POPN, .maybeOperand1 = 0},
        {.type = OP_DEFINE_GLOBAL_VAL, .maybeOperand1 = 1},
    };
    BytecodeArray expectedBytecodeArray = {.values = expectedBytecode, .used = 8};

    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.bytecodeArray));
    ASSERT(compiledCode.constantPool.values[0].as.number == 3);
    ASSERT(strcmp(compiledCode.constantPool.values[1].as.string, "a") == 0);

    FREE_ARRAY(compiledCode.bytecodeArray);
    FREE_ARRAY(compiledCode.constantPool);

    return SUCCESS_RETURN_CODE;
}

int test_compiler_block_expression_with_statements() {
    // val a = { val b = 2; 3; };
    Source testSource = {
        .rootStatements = {
            VAL_DECLARATION_STATEMENT(
                "a",
                BLOCK_EXPRESSION(
                    VAL_DECLARATION_STATEMENT("b", PRIMARY_EXPRESSION(NUMBER_LITERAL("2"))),
                    PRIMARY_EXPRESSION(NUMBER_LITERAL("3"))))},
        .numberOfStatements = 1,
    };

    COMPILE_TEST_SOURCE

    Bytecode expectedBytecode[] = {
        {.type = OP_LOAD_CONSTANT, .maybeOperand1 = 0},
        {.type = OP_DEFINE_LOCAL_VAL_FAST},
        {.type = OP_LOAD_CONSTANT, .maybeOperand1 = 1},
        {.type = OP_SWAP, .maybeOperand1 = 1},
        {.type = OP_POPN, .maybeOperand1 = 1},
        {.type = OP_DEFINE_GLOBAL_VAL, .maybeOperand1 = 2},
    };
    BytecodeArray expectedBytecodeArray = {.values = expectedBytecode, .used = 6};

    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.bytecodeArray));
    ASSERT(compiledCode.constantPool.values[0].as.number == 2);
    ASSERT(compiledCode.constantPool.values[1].as.number == 3);
    ASSERT(strcmp(compiledCode.constantPool.values[2].as.string, "a") == 0);

    FREE_ARRAY(compiledCode.bytecodeArray);
    FREE_ARRAY(compiledCode.constantPool);

    return SUCCESS_RETURN_CODE;
}