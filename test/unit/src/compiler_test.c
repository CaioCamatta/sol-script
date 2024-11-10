#include "compiler.h"

#include <string.h>

#include "../../minunit.h"
#include "debug.h"
#include "syntax.h"

// -------------------------------------------------------------------------------
// ---------------------- Macros to facilitate creating AST ----------------------
// -------------------------------------------------------------------------------

#define TOKEN(typeArg) \
    (Token) { .type = typeArg, .start = "_", .length = 1 }

#define IDENTIFIER_TOKEN(identifier) \
    (Token) { .type = TOKEN_IDENTIFIER, .start = identifier, .length = strlen(identifier) }

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

#define RETURN_STATEMENT(expressionInput)          \
    &(Statement) {                                 \
        .type = RETURN_STATEMENT,                  \
        .as.returnStatement = &(ReturnStatement) { \
            .expression = expressionInput          \
        }                                          \
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

#define VAR_DECLARATION_STATEMENT(identifierName, expressionInput)                                                      \
    &(Statement) {                                                                                                      \
        .type = VAR_DECLARATION_STATEMENT,                                                                              \
        .as.varDeclarationStatement = &(VarDeclarationStatement) {                                                      \
            .identifier = &(IdentifierLiteral){                                                                         \
                .token = (Token){.type = TOKEN_IDENTIFIER, .start = identifierName, .length = strlen(identifierName)}}, \
            .maybeExpression = expressionInput                                                                          \
        }                                                                                                               \
    }

#define ASSIGNMENT_STATEMENT(targetExpression, valueExpression) \
    &(Statement) {                                              \
        .type = ASSIGNMENT_STATEMENT,                           \
        .as.assignmentStatement = &(AssignmentStatement) {      \
            .target = targetExpression,                         \
            .value = valueExpression                            \
        }                                                       \
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

#define ITERATION_STATEMENT(conditionExpr, bodyStmt)     \
    &(Statement) {                                       \
        .type = ITERATION_STATEMENT,                     \
        .as.iterationStatement = &(IterationStatement) { \
            .conditionExpression = conditionExpr,        \
            .bodyStatement = bodyStmt                    \
        }                                                \
    }

#define LAMBDA_EXPRESSION(parametersArg, bodyExpression)                  \
    &(Expression) {                                                       \
        .type = LAMBDA_EXPRESSION,                                        \
        .as.lambdaExpression = &(LambdaExpression) {                      \
            .parameters = parametersArg,                                  \
            .bodyBlock = &(BlockExpression) {                             \
                .statementArray = (StatementArray){.used = 0, .size = 0}, \
                .lastExpression = bodyExpression                          \
            }                                                             \
        }                                                                 \
    }

#define CALL_EXPRESSION(leftHandSideExpr, argumentsArg) \
    &(Expression) {                                     \
        .type = CALL_EXPRESSION,                        \
        .as.callExpression = &(CallExpression) {        \
            .leftHandSide = (leftHandSideExpr),         \
            .arguments = (argumentsArg)                 \
        }                                               \
    }

// Helper macro for creating IdentifierArray
#define IDENTIFIER_ARRAY(...)                                                           \
    &(IdentifierArray) {                                                                \
        .values = (IdentifierLiteral[]){__VA_ARGS__},                                   \
        .used = sizeof((IdentifierLiteral[]){__VA_ARGS__}) / sizeof(IdentifierLiteral), \
        .size = sizeof((IdentifierLiteral[]){__VA_ARGS__}) / sizeof(IdentifierLiteral)  \
    }

// Helper macro for creating ExpressionArray
#define EXPRESSION_ARRAY(...)                                               \
    &(ExpressionArray) {                                                    \
        .values = (Expression*[]){__VA_ARGS__},                             \
        .used = sizeof((Expression*[]){__VA_ARGS__}) / sizeof(Expression*), \
        .size = sizeof((Expression*[]){__VA_ARGS__}) / sizeof(Expression*)  \
    }

#define STRUCT_EXPRESSION(...)                                                                    \
    &(Expression) {                                                                               \
        .type = STRUCT_EXPRESSION,                                                                \
        .as.structExpression = &(StructExpression) {                                              \
            .declarationArray = (StructDeclarationArray) {                                        \
                .values = (StructDeclaration*[]){__VA_ARGS__},                                    \
                .used = sizeof((StructDeclaration*[]){__VA_ARGS__}) / sizeof(StructDeclaration*), \
                .size = sizeof((StructDeclaration*[]){__VA_ARGS__}) / sizeof(StructDeclaration*)  \
            }                                                                                     \
        }                                                                                         \
    }

#define STRUCT_DECLARATION(id, expr)                                       \
    &(StructDeclaration) {                                                 \
        .isPrototype = false,                                              \
        .identifier = &(IdentifierLiteral){.token = IDENTIFIER_TOKEN(id)}, \
        .maybeExpression = expr                                            \
    }

#define MEMBER_EXPRESSION(left, right)               \
    &(Expression) {                                  \
        .type = MEMBER_EXPRESSION,                   \
        .as.memberExpression = &(MemberExpression) { \
            .leftHandSide = left,                    \
            .rightHandSide = right                   \
        }                                            \
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
#define COMPILE_TEST_SOURCE                              \
    CompilerState compilerState;                         \
    initCompilerState(&compilerState, &testSource);      \
    CompiledCode compiledCode = compile(&compilerState); \
    printCompiledCode(compiledCode);

// Macro to free the compiler state (assumed to exist in lexical scope)
#define FREE_COMPILER freeCompilerState(&compilerState);

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
    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.topLevelCodeObject.bytecodeArray));
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[0].as.number == 5);       // 5 is in slot 0 of the pool
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[0].maybeOperand1 == 0);  // first instruction loads slot 0
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[1].as.number == 7);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[1].maybeOperand1 == 1);

    // Clean up
    FREE_COMPILER

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
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.used == 2);                           // Check if two bytecode instructions are generated
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[0].type == OP_LOAD_CONSTANT);  // First should be OP_CONSTANT
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[1].type == OP_PRINT);          // Second should be OP_PRINT

    // Clean up
    FREE_COMPILER

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
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.used == 2);                               // Check if two bytecode instructions are generated
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[0].type == OP_LOAD_CONSTANT);      // First should be OP_LOAD_CONSTANT
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[1].type == OP_DEFINE_GLOBAL_VAL);  // Second should be OP_SET_GLOBAL_VAL

    FREE_COMPILER

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

    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[0].type == OP_LOAD_CONSTANT);      // load 42 into stack
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[1].type == OP_DEFINE_GLOBAL_VAL);  // assign top of stack to variable 'x'
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[2].type == OP_GET_GLOBAL_VAL);     // get variable 'x'
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[3].type == OP_PRINT);              // print top of stack

    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[0].type == CONST_TYPE_DOUBLE);  // 42
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[0].as.number == 42);
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[1].type == CONST_TYPE_IDENTIFIER);  // x

    FREE_COMPILER

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
    ASSERT(compiledCode.topLevelCodeObject.constantPool.used == 3);

    // Cleanup
    FREE_COMPILER

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
    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.topLevelCodeObject.bytecodeArray));

    // Cleanup
    FREE_COMPILER

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
    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.topLevelCodeObject.bytecodeArray));

    // Cleanup
    FREE_COMPILER

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
    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.topLevelCodeObject.bytecodeArray));

    // Cleanup
    FREE_COMPILER

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
    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.topLevelCodeObject.bytecodeArray));

    // Cleanup
    FREE_COMPILER

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
    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.topLevelCodeObject.bytecodeArray));

    // Verify constants in the constant pool
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[0].as.number == 5);
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[1].as.number == 2);
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[2].as.number == 10);

    // Cleanup
    FREE_COMPILER

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
    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.topLevelCodeObject.bytecodeArray));

    // Clean up
    FREE_COMPILER

    return SUCCESS_RETURN_CODE;
}

int test_compiler_boolean_literal() {
    Source testSource = (Source){
        .rootStatements = {
            EXPRESSION_STATEMENT(PRIMARY_EXPRESSION(BOOLEAN_LITERAL(false)))},
        .numberOfStatements = 1,
    };

    COMPILE_TEST_SOURCE

    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.used == 2);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[0].type == OP_FALSE);

    FREE_COMPILER

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
    ASSERT(compiledCode.topLevelCodeObject.constantPool.used == 1);                                          // Ensure constant pool has entries
    ASSERT(strcmp(compiledCode.topLevelCodeObject.constantPool.values[0].as.string, "Hello, World!") == 0);  // Check string content
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[0].type == OP_LOAD_CONSTANT);                // Check bytecode to load string

    // Cleanup
    FREE_COMPILER

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
    ASSERT(compilerState.currentCompilerUnit.predictedStack.currentStackHeight == 0);

    // Clean up
    FREE_COMPILER

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
    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.topLevelCodeObject.bytecodeArray));

    // Cleanup
    FREE_COMPILER

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
    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.topLevelCodeObject.bytecodeArray));

    // Assert constant pool contains one identifier for the global, and four numbers.
    //  Constant Pool:
    //  #0 (double) 100.000000
    //  #1 (identifier) "g"
    //  #2 (double) 10.000000
    //  #3 (double) 20.000000
    //  #4 (double) 30.000000
    ASSERT(compiledCode.topLevelCodeObject.constantPool.used == 5);
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[0].type == CONST_TYPE_DOUBLE);
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[0].as.number == 100.0);
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[1].type == CONST_TYPE_IDENTIFIER);
    ASSERT(strcmp(compiledCode.topLevelCodeObject.constantPool.values[1].as.string, "g") == 0);
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[2].type == CONST_TYPE_DOUBLE);
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[2].as.number == 10.0);
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[3].type == CONST_TYPE_DOUBLE);
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[3].as.number == 20.0);
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[4].type == CONST_TYPE_DOUBLE);
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[4].as.number == 30.0);

    // Cleanup and assertions
    FREE_COMPILER

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
    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.topLevelCodeObject.bytecodeArray));

    // Check that nothing is popped (we didnt create any variables)
    ASSERT(expectedBytecode[4].maybeOperand1 == compiledCode.topLevelCodeObject.bytecodeArray.values[4].maybeOperand1);

    // Cleanup
    FREE_COMPILER

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
    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.topLevelCodeObject.bytecodeArray));

    // Check that nothing is popped (we didnt create any variables)
    ASSERT(expectedBytecode[4].maybeOperand1 == compiledCode.topLevelCodeObject.bytecodeArray.values[4].maybeOperand1);
    ASSERT(expectedBytecode[8].maybeOperand1 == compiledCode.topLevelCodeObject.bytecodeArray.values[8].maybeOperand1);

    // Cleanup
    FREE_COMPILER

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
    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.topLevelCodeObject.bytecodeArray));

    // Verify operands
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[1].maybeOperand1 == 15);   // Operand for first OP_JUMP_IF_FALSE
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[5].maybeOperand1 == 10);   // Operand for nested OP_JUMP_IF_FALSE
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[9].maybeOperand1 == 13);   // Operand for first OP_JUMP
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[14].maybeOperand1 == 18);  // Operand for second OP_JUMP
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[8].maybeOperand1 == 0);    // Operand for the first OP_POPN after "true-inner"
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[12].maybeOperand1 == 0);   // Operand for the OP_POPN after "false-inner"
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[13].maybeOperand1 == 0);   // Operand for the OP_POPN after exiting the inner if
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[17].maybeOperand1 == 0);   // Operand for the OP_POPN after "false-outer"

    // Clean up
    FREE_COMPILER

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
        {.type = OP_POPN, .maybeOperand1 = 0},
        {.type = OP_DEFINE_GLOBAL_VAL, .maybeOperand1 = 1},
    };
    BytecodeArray expectedBytecodeArray = {.values = expectedBytecode, .used = 3};

    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.topLevelCodeObject.bytecodeArray));
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[0].as.number == 3);
    ASSERT(strcmp(compiledCode.topLevelCodeObject.constantPool.values[1].as.string, "a") == 0);

    FREE_COMPILER

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
        {.type = OP_POPN, .maybeOperand1 = 0},
        {.type = OP_POPN, .maybeOperand1 = 0},
        {.type = OP_POPN, .maybeOperand1 = 0},
        {.type = OP_DEFINE_GLOBAL_VAL, .maybeOperand1 = 1},
    };
    BytecodeArray expectedBytecodeArray = {.values = expectedBytecode, .used = 5};

    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.topLevelCodeObject.bytecodeArray));
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[0].as.number == 3);
    ASSERT(strcmp(compiledCode.topLevelCodeObject.constantPool.values[1].as.string, "a") == 0);

    FREE_COMPILER

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

    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.topLevelCodeObject.bytecodeArray));
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[0].as.number == 2);
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[1].as.number == 3);
    ASSERT(strcmp(compiledCode.topLevelCodeObject.constantPool.values[2].as.string, "a") == 0);

    FREE_COMPILER

    return SUCCESS_RETURN_CODE;
}

int test_compiler_var_declaration_and_assignment_global() {
    Source testSource = {
        .rootStatements = {
            VAR_DECLARATION_STATEMENT("a", NULL),
            ASSIGNMENT_STATEMENT(PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("a")), PRIMARY_EXPRESSION(NUMBER_LITERAL("10"))),
            PRINT_STATEMENT(PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("a")))},
        .numberOfStatements = 3,
    };

    COMPILE_TEST_SOURCE

    // Verify bytecode
    Bytecode expectedBytecode[] = {
        BYTECODE(OP_NULL),
        BYTECODE_OPERAND_1(OP_DEFINE_GLOBAL_VAR, 0),
        BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 1),
        BYTECODE_OPERAND_1(OP_SET_GLOBAL_VAR, 0),
        BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAR, 0),
        BYTECODE(OP_PRINT),
    };
    BytecodeArray expectedBytecodeArray = {.values = expectedBytecode, .used = 6};

    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.topLevelCodeObject.bytecodeArray));

    // Clean up
    FREE_COMPILER

    return SUCCESS_RETURN_CODE;
}

// Test var declaration and assignment in local scope
int test_compiler_var_declaration_and_assignment_local() {
    Source testSource = {
        .rootStatements = {
            BLOCK_STATEMENT(
                VAR_DECLARATION_STATEMENT("a", NULL),
                ASSIGNMENT_STATEMENT(PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("a")), PRIMARY_EXPRESSION(NUMBER_LITERAL("10"))),
                PRINT_STATEMENT(PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("a"))))},
        .numberOfStatements = 1,
    };

    COMPILE_TEST_SOURCE

    // Verify bytecode
    Bytecode expectedBytecode[] = {
        BYTECODE(OP_NULL),
        BYTECODE(OP_DEFINE_LOCAL_VAR_FAST),
        BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0),
        BYTECODE_OPERAND_1(OP_SET_LOCAL_VAR_FAST, 0),
        BYTECODE_OPERAND_1(OP_GET_LOCAL_VAR_FAST, 0),
        BYTECODE(OP_PRINT),
        BYTECODE_OPERAND_1(OP_POPN, 1),
    };
    BytecodeArray expectedBytecodeArray = {.values = expectedBytecode, .used = 7};

    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.topLevelCodeObject.bytecodeArray));

    // Clean up
    FREE_COMPILER

    return SUCCESS_RETURN_CODE;
}

int test_compiler_global_declaration_and_local_assignment() {
    Source testSource = {
        .rootStatements = {
            VAR_DECLARATION_STATEMENT("a", NULL),
            BLOCK_STATEMENT(
                ASSIGNMENT_STATEMENT(PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("a")), PRIMARY_EXPRESSION(NUMBER_LITERAL("1"))),
                PRINT_STATEMENT(PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("a")))),
            PRINT_STATEMENT(PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("a")))},
        .numberOfStatements = 3,
    };

    COMPILE_TEST_SOURCE

    // Verify bytecode
    Bytecode expectedBytecode[] = {
        BYTECODE(OP_NULL),
        BYTECODE_OPERAND_1(OP_DEFINE_GLOBAL_VAR, 0),
        BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 1),
        BYTECODE_OPERAND_1(OP_SET_GLOBAL_VAR, 0),
        BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAR, 0),
        BYTECODE(OP_PRINT),
        BYTECODE_OPERAND_1(OP_POPN, 0),
        BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAR, 0),
        BYTECODE(OP_PRINT),
    };
    BytecodeArray expectedBytecodeArray = {.values = expectedBytecode, .used = 9};

    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.topLevelCodeObject.bytecodeArray));

    // Verify constant pool
    ASSERT(compiledCode.topLevelCodeObject.constantPool.used == 2);
    ASSERT(strcmp(compiledCode.topLevelCodeObject.constantPool.values[0].as.string, "a") == 0);
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[1].as.number == 1);

    // Clean up
    FREE_COMPILER

    return SUCCESS_RETURN_CODE;
}

int test_compiler_iteration_statement_simple() {
    // while (true) print "Loop";
    Source testSource = {
        .rootStatements = {
            ITERATION_STATEMENT(
                PRIMARY_EXPRESSION(BOOLEAN_LITERAL(true)),
                PRINT_STATEMENT(PRIMARY_EXPRESSION(STRING_LITERAL("\"Loop\""))))},
        .numberOfStatements = 1,
    };

    COMPILE_TEST_SOURCE

    Bytecode expectedBytecode[] = {
        BYTECODE(OP_TRUE),  // Condition
        BYTECODE_OPERAND_1(OP_JUMP_IF_FALSE, 7),
        BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0),
        BYTECODE(OP_PRINT),
        BYTECODE_OPERAND_1(OP_JUMP, 0),  // Jump to condition
    };
    BytecodeArray expectedBytecodeArray = {.values = expectedBytecode, .used = sizeof(expectedBytecode) / sizeof(Bytecode)};

    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.topLevelCodeObject.bytecodeArray));

    FREE_COMPILER

    return SUCCESS_RETURN_CODE;
}

int test_compiler_iteration_statement_with_variable() {
    // var i = 0; while (i < 3) { print i; i = i + 1; }
    Source testSource = {
        .rootStatements = {
            VAR_DECLARATION_STATEMENT("i", PRIMARY_EXPRESSION(NUMBER_LITERAL("0"))),
            ITERATION_STATEMENT(
                COMPARISON_EXPRESSION(
                    PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("i")),
                    PRIMARY_EXPRESSION(NUMBER_LITERAL("3")),
                    TOKEN(TOKEN_LESSER)),
                BLOCK_STATEMENT(
                    PRINT_STATEMENT(PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("i"))),
                    ASSIGNMENT_STATEMENT(
                        PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("i")),
                        ADDITIVE_EXPRESSION(
                            PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("i")),
                            TOKEN(TOKEN_PLUS),
                            PRIMARY_EXPRESSION(NUMBER_LITERAL("1"))))))},
        .numberOfStatements = 2,
    };

    COMPILE_TEST_SOURCE

    Bytecode expectedBytecode[] = {
        BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0),   // Load 0
        BYTECODE(OP_DEFINE_GLOBAL_VAR),            // Define variable 'i'
        BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAR, 1),  // Get 'i'
        BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 2),   // Load 3
        BYTECODE(OP_BINARY_LT),                    // Compare 'i' < 3
        BYTECODE_OPERAND_1(OP_JUMP_IF_FALSE, 15),  // Jump to end if false
        BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAR, 1),  // Get 'i'
        BYTECODE(OP_PRINT),                        // Print 'i'
        BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAR, 1),  // Get 'i'
        BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 3),   // Load 1
        BYTECODE(OP_BINARY_ADD),                   // Add 'i' + 1
        BYTECODE_OPERAND_1(OP_SET_GLOBAL_VAR, 1),  // Set 'i' to 'i' + 1
        BYTECODE_OPERAND_1(OP_POPN, 0),            // Clean up the stack
        BYTECODE_OPERAND_1(OP_JUMP, 2),            // Jump back to condition
    };
    BytecodeArray expectedBytecodeArray = {.values = expectedBytecode, .used = sizeof(expectedBytecode) / sizeof(Bytecode)};
    ASSERT(compareTypesInBytecodeArrays(expectedBytecodeArray, compiledCode.topLevelCodeObject.bytecodeArray));

    FREE_COMPILER

    return SUCCESS_RETURN_CODE;
}

int test_compiler_lambda_expression_simple() {
    Source testSource = {
        .rootStatements = {
            VAL_DECLARATION_STATEMENT(
                "add",
                LAMBDA_EXPRESSION(
                    IDENTIFIER_ARRAY(
                        {.token = IDENTIFIER_TOKEN("a")},
                        {.token = IDENTIFIER_TOKEN("b")}),
                    ADDITIVE_EXPRESSION(
                        PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("a")),
                        TOKEN(TOKEN_PLUS),
                        PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("b")))))},
        .numberOfStatements = 1,
    };

    COMPILE_TEST_SOURCE

    // Verify the constant pool contains the function
    ASSERT(compiledCode.topLevelCodeObject.constantPool.used == 2);  // 'add' and the function object
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[0].type == CONST_TYPE_LAMBDA);

    // Verify the bytecode for defining the function
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[0].type == OP_LAMBDA);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[1].type == OP_DEFINE_GLOBAL_VAL);

    // Verify the function's bytecode
    Function* function = compiledCode.topLevelCodeObject.constantPool.values[0].as.lambda;
    ASSERT(function->parameterCount == 2);
    ASSERT(function->code->bytecodeArray.values[0].type == OP_GET_LOCAL_VAL_FAST);
    ASSERT(function->code->bytecodeArray.values[1].type == OP_GET_LOCAL_VAL_FAST);
    ASSERT(function->code->bytecodeArray.values[2].type == OP_BINARY_ADD);
    ASSERT(function->code->bytecodeArray.values[3].type == OP_POPN);
    ASSERT(function->code->bytecodeArray.values[4].type == OP_RETURN);

    FREE_COMPILER

    return SUCCESS_RETURN_CODE;
}

int test_compiler_lambda_expression_nested() {
    Source testSource = {
        .rootStatements = {
            VAL_DECLARATION_STATEMENT(
                "makeAdder",
                LAMBDA_EXPRESSION(
                    IDENTIFIER_ARRAY(
                        {.token = IDENTIFIER_TOKEN("x")}),
                    LAMBDA_EXPRESSION(
                        IDENTIFIER_ARRAY(
                            {.token = IDENTIFIER_TOKEN("y")}),
                        ADDITIVE_EXPRESSION(
                            PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("y")),
                            TOKEN(TOKEN_PLUS),
                            PRIMARY_EXPRESSION(NUMBER_LITERAL("1"))))))},
        .numberOfStatements = 1,
    };

    COMPILE_TEST_SOURCE

    // Verify the constant pool contains the outer function
    ASSERT(compiledCode.topLevelCodeObject.constantPool.used == 2);  // 'makeAdder' and the outer function object
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[0].type == CONST_TYPE_LAMBDA);

    // Verify the bytecode for defining the outer function
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[0].type == OP_LAMBDA);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[1].type == OP_DEFINE_GLOBAL_VAL);

    // Verify the outer function's bytecode
    Function* outerFunction = compiledCode.topLevelCodeObject.constantPool.values[0].as.lambda;
    ASSERT(outerFunction->parameterCount == 1);
    ASSERT(outerFunction->code->bytecodeArray.values[0].type == OP_LAMBDA);
    ASSERT(outerFunction->code->bytecodeArray.values[1].type == OP_POPN);
    ASSERT(outerFunction->code->bytecodeArray.values[2].type == OP_RETURN);

    // Verify the inner function's bytecode
    Function* innerFunction = outerFunction->code->constantPool.values[0].as.lambda;
    ASSERT(innerFunction->parameterCount == 1);
    ASSERT(innerFunction->code->bytecodeArray.values[0].type == OP_GET_LOCAL_VAL_FAST);  // y
    ASSERT(innerFunction->code->bytecodeArray.values[1].type == OP_LOAD_CONSTANT);       // 1
    ASSERT(innerFunction->code->bytecodeArray.values[2].type == OP_BINARY_ADD);
    ASSERT(innerFunction->code->bytecodeArray.values[3].type == OP_POPN);
    ASSERT(innerFunction->code->bytecodeArray.values[4].type == OP_RETURN);

    FREE_COMPILER

    return SUCCESS_RETURN_CODE;
}

int test_compiler_call_expression_simple() {
    Source testSource = {
        .rootStatements = {
            VAL_DECLARATION_STATEMENT(
                "add",
                LAMBDA_EXPRESSION(
                    IDENTIFIER_ARRAY(
                        {.token = IDENTIFIER_TOKEN("a")},
                        {.token = IDENTIFIER_TOKEN("b")}),
                    ADDITIVE_EXPRESSION(
                        PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("a")),
                        TOKEN(TOKEN_PLUS),
                        PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("b"))))),
            VAL_DECLARATION_STATEMENT(
                "result",
                CALL_EXPRESSION(
                    PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("add")),
                    EXPRESSION_ARRAY(
                        PRIMARY_EXPRESSION(NUMBER_LITERAL("5")),
                        PRIMARY_EXPRESSION(NUMBER_LITERAL("3")))))},
        .numberOfStatements = 2,
    };

    COMPILE_TEST_SOURCE

    // Verify the constant pool
    ASSERT(compiledCode.topLevelCodeObject.constantPool.used == 5);  // 'add', lambda, 'result', 5, and 3

    // Verify the bytecode for lambda definition
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[0].type == OP_LAMBDA);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[1].type == OP_DEFINE_GLOBAL_VAL);  // 'add'

    // Verify the bytecode for function call
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[2].type == OP_LOAD_CONSTANT);   // 5
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[3].type == OP_LOAD_CONSTANT);   // 3
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[4].type == OP_GET_GLOBAL_VAL);  // 'add'
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[5].type == OP_CALL);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[6].type == OP_DEFINE_GLOBAL_VAL);  // 'result'

    FREE_COMPILER

    return SUCCESS_RETURN_CODE;
}

int test_compiler_call_expression_nested() {
    Source testSource = {
        .rootStatements = {
            VAL_DECLARATION_STATEMENT(
                "add",
                LAMBDA_EXPRESSION(
                    IDENTIFIER_ARRAY(
                        {.token = IDENTIFIER_TOKEN("a")},
                        {.token = IDENTIFIER_TOKEN("b")}),
                    ADDITIVE_EXPRESSION(
                        PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("a")),
                        TOKEN(TOKEN_PLUS),
                        PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("b"))))),
            VAL_DECLARATION_STATEMENT(
                "multiply",
                LAMBDA_EXPRESSION(
                    IDENTIFIER_ARRAY(
                        {.token = IDENTIFIER_TOKEN("a")},
                        {.token = IDENTIFIER_TOKEN("b")}),
                    MULTIPLICATIVE_EXPRESSION(
                        PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("a")),
                        TOKEN(TOKEN_STAR),
                        PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("b"))))),
            VAL_DECLARATION_STATEMENT(
                "result",
                CALL_EXPRESSION(PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("add")),
                                EXPRESSION_ARRAY(
                                    PRIMARY_EXPRESSION(NUMBER_LITERAL("5")),
                                    CALL_EXPRESSION(PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("multiply")),
                                                    EXPRESSION_ARRAY(
                                                        PRIMARY_EXPRESSION(NUMBER_LITERAL("3")),
                                                        PRIMARY_EXPRESSION(NUMBER_LITERAL("2")))))))},
        .numberOfStatements = 3,
    };

    COMPILE_TEST_SOURCE

    // Verify the constant pool
    ASSERT(compiledCode.topLevelCodeObject.constantPool.used == 8);  // 'add', 'multiply', their lambdas, 'result', 5, 3, and 2

    // Verify the bytecode for lambda definitions
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[0].type == OP_LAMBDA);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[1].type == OP_DEFINE_GLOBAL_VAL);  // 'add'
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[2].type == OP_LAMBDA);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[3].type == OP_DEFINE_GLOBAL_VAL);  // 'multiply'

    // Verify the bytecode for the nested function calls
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[4].type == OP_LOAD_CONSTANT);   // 5
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[5].type == OP_LOAD_CONSTANT);   // 3
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[6].type == OP_LOAD_CONSTANT);   // 2
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[7].type == OP_GET_GLOBAL_VAL);  // 'multiply'
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[8].type == OP_CALL);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[9].type == OP_GET_GLOBAL_VAL);  // 'add'
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[10].type == OP_CALL);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[11].type == OP_DEFINE_GLOBAL_VAL);  // 'result'

    FREE_COMPILER

    return SUCCESS_RETURN_CODE;
}

int test_compiler_chained_calls() {
    Source testSource = {
        .rootStatements = {
            VAL_DECLARATION_STATEMENT(
                "add",
                LAMBDA_EXPRESSION(
                    IDENTIFIER_ARRAY(
                        {.token = IDENTIFIER_TOKEN("a")},
                        {.token = IDENTIFIER_TOKEN("b")}),
                    ADDITIVE_EXPRESSION(
                        PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("a")),
                        TOKEN(TOKEN_PLUS),
                        PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("b"))))),
            EXPRESSION_STATEMENT(
                CALL_EXPRESSION(
                    CALL_EXPRESSION(
                        CALL_EXPRESSION(
                            PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("add")),
                            EXPRESSION_ARRAY(
                                PRIMARY_EXPRESSION(NUMBER_LITERAL("1")),
                                PRIMARY_EXPRESSION(NUMBER_LITERAL("2")))),
                        EXPRESSION_ARRAY(
                            PRIMARY_EXPRESSION(NUMBER_LITERAL("3")),
                            PRIMARY_EXPRESSION(NUMBER_LITERAL("4")))),
                    EXPRESSION_ARRAY(
                        PRIMARY_EXPRESSION(NUMBER_LITERAL("5")),
                        PRIMARY_EXPRESSION(NUMBER_LITERAL("6")))))},
        .numberOfStatements = 2,
    };

    COMPILE_TEST_SOURCE

    // Verify the constant pool
    ASSERT(compiledCode.topLevelCodeObject.constantPool.used == 8);  // 'add', 1, 2, 3, 4, 5, 6
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[0].type == CONST_TYPE_LAMBDA);
    ASSERT(strcmp(compiledCode.topLevelCodeObject.constantPool.values[1].as.string, "add") == 0);
    for (int i = 2; i <= 7; i++) {
        ASSERT(compiledCode.topLevelCodeObject.constantPool.values[i].type == CONST_TYPE_DOUBLE);
    }

    // Verify the bytecode for the chained function calls
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[0].type == OP_LAMBDA);             // 'add'
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[1].type == OP_DEFINE_GLOBAL_VAL);  // 'add'
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[2].type == OP_LOAD_CONSTANT);      // 1
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[3].type == OP_LOAD_CONSTANT);      // 2
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[4].type == OP_LOAD_CONSTANT);      // 3
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[5].type == OP_LOAD_CONSTANT);      // 4
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[6].type == OP_LOAD_CONSTANT);      // 5
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[7].type == OP_LOAD_CONSTANT);      // 6
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[8].type == OP_GET_GLOBAL_VAL);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[9].type == OP_CALL);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[10].type == OP_CALL);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[11].type == OP_CALL);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[12].type == OP_POPN);

    FREE_COMPILER

    return SUCCESS_RETURN_CODE;
}

int test_compiler_lambda_with_return() {
    Source testSource = {
        .rootStatements = {
            VAL_DECLARATION_STATEMENT(
                "func",
                LAMBDA_EXPRESSION(
                    IDENTIFIER_ARRAY(),
                    BLOCK_EXPRESSION(
                        RETURN_STATEMENT(PRIMARY_EXPRESSION(NUMBER_LITERAL("10"))),
                        NULL)))},
        .numberOfStatements = 1,
    };

    COMPILE_TEST_SOURCE

    ASSERT(compiledCode.topLevelCodeObject.constantPool.used == 2);  // 'func' and the lambda
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[0].type == CONST_TYPE_LAMBDA);
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[1].type == CONST_TYPE_IDENTIFIER);

    Function* lambda = compiledCode.topLevelCodeObject.constantPool.values[0].as.lambda;
    ASSERT(lambda->parameterCount == 0);

    Bytecode expectedLambdaBytecode[] = {
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_RETURN},
        {.type = OP_NULL},
        {.type = OP_SWAP},
        {.type = OP_POPN},
        {.type = OP_POPN},
        {.type = OP_RETURN},
    };
    BytecodeArray expectedLambdaBytecodeArray = {.values = expectedLambdaBytecode, .used = 7};

    ASSERT(compareTypesInBytecodeArrays(expectedLambdaBytecodeArray, lambda->code->bytecodeArray));
    ASSERT(lambda->code->constantPool.values[0].as.number == 10);

    FREE_COMPILER

    return SUCCESS_RETURN_CODE;
}

int test_compiler_lambda_with_conditional_returns() {
    Source testSource = {
        .rootStatements = {
            VAL_DECLARATION_STATEMENT(
                "func",
                LAMBDA_EXPRESSION(
                    IDENTIFIER_ARRAY({.token = IDENTIFIER_TOKEN("x")}),
                    BLOCK_EXPRESSION(
                        SELECTION_STATEMENT(
                            COMPARISON_EXPRESSION(
                                PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("x")),
                                PRIMARY_EXPRESSION(NUMBER_LITERAL("0")),
                                TOKEN(TOKEN_GREATER)),
                            BLOCK_STATEMENT(RETURN_STATEMENT(PRIMARY_EXPRESSION(NUMBER_LITERAL("1")))),
                            BLOCK_STATEMENT(RETURN_STATEMENT(UNARY_EXPRESSION(TOKEN(TOKEN_MINUS), PRIMARY_EXPRESSION(NUMBER_LITERAL("1")))))),
                        NULL)))},
        .numberOfStatements = 1,
    };

    COMPILE_TEST_SOURCE

    ASSERT(compiledCode.topLevelCodeObject.constantPool.used == 2);  // 'func' and the lambda
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[0].type == CONST_TYPE_LAMBDA);
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[1].type == CONST_TYPE_IDENTIFIER);

    Function* lambda = compiledCode.topLevelCodeObject.constantPool.values[0].as.lambda;
    ASSERT(lambda->parameterCount == 1);

    Bytecode expectedLambdaBytecode[] = {
        {.type = OP_GET_LOCAL_VAL_FAST},
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_BINARY_GT},
        {.type = OP_JUMP_IF_FALSE},
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_RETURN},
        {.type = OP_POPN},
        {.type = OP_JUMP},
        {.type = OP_LOAD_CONSTANT},
        {.type = OP_UNARY_NEGATE},
        {.type = OP_RETURN},
        {.type = OP_POPN},
        {.type = OP_NULL},
        {.type = OP_POPN},
        {.type = OP_POPN},
        {.type = OP_RETURN},
    };
    BytecodeArray expectedLambdaBytecodeArray = {.values = expectedLambdaBytecode, .used = 16};

    ASSERT(compareTypesInBytecodeArrays(expectedLambdaBytecodeArray, lambda->code->bytecodeArray));
    ASSERT(lambda->code->constantPool.values[0].as.number == 0);
    ASSERT(lambda->code->constantPool.values[1].as.number == 1);

    FREE_COMPILER

    return SUCCESS_RETURN_CODE;
}

int test_compiler_simple_struct() {
    Source testSource = {
        .rootStatements = {
            EXPRESSION_STATEMENT(
                STRUCT_EXPRESSION(
                    STRUCT_DECLARATION("x", PRIMARY_EXPRESSION(NUMBER_LITERAL("42"))),
                    STRUCT_DECLARATION("y", PRIMARY_EXPRESSION(STRING_LITERAL("\"hello\"")))))},
        .numberOfStatements = 1,
    };

    COMPILE_TEST_SOURCE

    // Verify bytecode
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[0].type == OP_NEW_STRUCT);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[1].type == OP_LOAD_CONSTANT);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[2].type == OP_SET_FIELD);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[3].type == OP_LOAD_CONSTANT);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[4].type == OP_SET_FIELD);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[5].type == OP_POPN);

    // Verify constant pool
    ASSERT(compiledCode.topLevelCodeObject.constantPool.used == 4);
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[0].type == CONST_TYPE_DOUBLE);
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[0].as.number == 42);
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[1].type == CONST_TYPE_STRING);
    ASSERT(strcmp(compiledCode.topLevelCodeObject.constantPool.values[1].as.string, "x") == 0);
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[2].type == CONST_TYPE_STRING);
    ASSERT(strcmp(compiledCode.topLevelCodeObject.constantPool.values[2].as.string, "hello") == 0);
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[3].type == CONST_TYPE_STRING);
    ASSERT(strcmp(compiledCode.topLevelCodeObject.constantPool.values[3].as.string, "y") == 0);

    FREE_COMPILER
    return SUCCESS_RETURN_CODE;
}

int test_compiler_struct_field_access() {
    Source testSource = {
        .rootStatements = {
            VAR_DECLARATION_STATEMENT("myStruct",
                                      STRUCT_EXPRESSION(
                                          STRUCT_DECLARATION("field", PRIMARY_EXPRESSION(NUMBER_LITERAL("123"))))),
            PRINT_STATEMENT(
                MEMBER_EXPRESSION(
                    PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("myStruct")),
                    PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("field"))))},
        .numberOfStatements = 2,
    };

    COMPILE_TEST_SOURCE

    // Verify bytecode for struct creation and field access
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[0].type == OP_NEW_STRUCT);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[1].type == OP_LOAD_CONSTANT);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[2].type == OP_SET_FIELD);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[3].type == OP_DEFINE_GLOBAL_VAR);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[4].type == OP_GET_GLOBAL_VAR);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[5].type == OP_GET_FIELD);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[6].type == OP_PRINT);

    FREE_COMPILER
    return SUCCESS_RETURN_CODE;
}

int test_compiler_nested_struct() {
    Source testSource = {
        .rootStatements = {
            EXPRESSION_STATEMENT(
                STRUCT_EXPRESSION(
                    STRUCT_DECLARATION("outer",
                                       STRUCT_EXPRESSION(
                                           STRUCT_DECLARATION("inner", PRIMARY_EXPRESSION(NUMBER_LITERAL("42")))))))},
        .numberOfStatements = 1,
    };

    COMPILE_TEST_SOURCE

    // Verify bytecode for nested struct creation
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[0].type == OP_NEW_STRUCT);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[1].type == OP_NEW_STRUCT);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[2].type == OP_LOAD_CONSTANT);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[3].type == OP_SET_FIELD);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[4].type == OP_SET_FIELD);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[5].type == OP_POPN);

    // Verify constant pool
    ASSERT(compiledCode.topLevelCodeObject.constantPool.used == 3);
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[0].type == CONST_TYPE_DOUBLE);
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[0].as.number == 42);
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[1].type == CONST_TYPE_STRING);
    ASSERT(strcmp(compiledCode.topLevelCodeObject.constantPool.values[1].as.string, "inner") == 0);
    ASSERT(compiledCode.topLevelCodeObject.constantPool.values[2].type == CONST_TYPE_STRING);
    ASSERT(strcmp(compiledCode.topLevelCodeObject.constantPool.values[2].as.string, "outer") == 0);

    FREE_COMPILER
    return SUCCESS_RETURN_CODE;
}

int test_compiler_struct_assignment() {
    Source testSource = {
        .rootStatements = {
            VAR_DECLARATION_STATEMENT("myStruct",
                                      STRUCT_EXPRESSION(
                                          STRUCT_DECLARATION("field", PRIMARY_EXPRESSION(NUMBER_LITERAL("123"))))),
            ASSIGNMENT_STATEMENT(
                MEMBER_EXPRESSION(
                    PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("myStruct")),
                    PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("field"))),
                PRIMARY_EXPRESSION(NUMBER_LITERAL("456")))},
        .numberOfStatements = 2,
    };

    COMPILE_TEST_SOURCE

    // Verify bytecode for struct creation and field assignment
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[0].type == OP_NEW_STRUCT);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[1].type == OP_LOAD_CONSTANT);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[2].type == OP_SET_FIELD);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[3].type == OP_DEFINE_GLOBAL_VAR);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[4].type == OP_GET_GLOBAL_VAR);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[5].type == OP_LOAD_CONSTANT);
    ASSERT(compiledCode.topLevelCodeObject.bytecodeArray.values[6].type == OP_SET_FIELD);

    FREE_COMPILER
    return SUCCESS_RETURN_CODE;
}