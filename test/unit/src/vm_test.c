#include "vm.h"

#include <stdio.h>
#include <stdlib.h>

#include "../../minunit.h"
#include "../../utils.h"
#include "bytecode.h"

// Helper function to compare values
int compareValues(Value a, Value b) {
    if (a.type != b.type) {
        return 0;
    }

    switch (a.type) {
        case TYPE_DOUBLE:
            return a.as.doubleVal == b.as.doubleVal;
            // Add cases for other types as needed

        default:
            return 0;  // Unknown type
    }
}

// Create new compiled code object
CompiledCodeObject newCompiledCodeObject() {
    CompiledCodeObject compiledCodeObject = (CompiledCodeObject){
        .constantPool = (ConstantPool){},
        .bytecodeArray = (BytecodeArray){}};
    INIT_ARRAY(compiledCodeObject.bytecodeArray, Bytecode);
    INIT_ARRAY(compiledCodeObject.constantPool, Value);
    return compiledCodeObject;
}

// Create new empty compiled code
CompiledCode newCompiledCode() {
    CompiledCode compiledCode = (CompiledCode){
        .topLevelCodeObject = newCompiledCodeObject()};
    return compiledCode;
}

// Free compiled code. Used only in the context of these tests
static void _freeCompiledCode(CompiledCode* compiledCode) {
    FREE_ARRAY(compiledCode->topLevelCodeObject.bytecodeArray);
    FREE_ARRAY(compiledCode->topLevelCodeObject.constantPool);
}

// Add multiple bytecodes to an array
void addBytecodes(BytecodeArray* array, Bytecode* bytecodes, size_t count) {
    for (size_t i = 0; i < count; ++i) {
        INSERT_ARRAY((*array), bytecodes[i], Bytecode);
    }
}
#define ADD_BYTECODES(array, ...)                                 \
    do {                                                          \
        Bytecode tmp[] = {__VA_ARGS__};                           \
        addBytecodes(array, tmp, sizeof(tmp) / sizeof(Bytecode)); \
    } while (0)

// Simplified pop function for testing
static Value popVmStack(VM* vm) {
    vm->currFrame->SP--;
    return *(vm->currFrame->SP);
}

void freeLambdaFunction(Function* function) {
    FREE_ARRAY(function->code->bytecodeArray);
    FREE_ARRAY(function->code->constantPool);
    free(function->code);
    free(function);
}

int test_vm_addition() {
    CompiledCode compiledCode = newCompiledCode();

    // Create a simple bytecode program: 1 + 2
    do {
        if (compiledCode.topLevelCodeObject.constantPool.used == compiledCode.topLevelCodeObject.constantPool.size) {
            compiledCode.topLevelCodeObject.constantPool.size *= 2;
            compiledCode.topLevelCodeObject.constantPool.values = realloc(compiledCode.topLevelCodeObject.constantPool.values, compiledCode.topLevelCodeObject.constantPool.size * sizeof(Constant));
        }
        compiledCode.topLevelCodeObject.constantPool.values[compiledCode.topLevelCodeObject.constantPool.used++] = (Constant){
            .type = CONST_TYPE_DOUBLE,
            .as = {.number = 1.0},
        };
    } while (0);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, DOUBLE_CONST(2.0), Constant);  // put Constant in index 1
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 1), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE(OP_BINARY_ADD), Bytecode);

    // Initialize VM with the bytecode
    VM vm;
    initVM(&vm, compiledCode);

    // Run the VM
    run(&vm);

    // Check the result on the stack
    Value expected_result = DOUBLE_VAL(3.0);
    Value actual_result = popVmStack(&vm);
    ASSERT(compareValues(actual_result, expected_result));

    // Clean up
    _freeCompiledCode(&compiledCode);

    return SUCCESS_RETURN_CODE;
}

int test_vm_print() {
    // Initialize the VM
    VM vm;
    CompiledCode compiledCode = newCompiledCode();

    // Setup bytecode for a print statement
    Constant numberToPrint = DOUBLE_CONST(42);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, numberToPrint, Constant);
    size_t indexOfNumberToPrintInPool = 0;

    Bytecode constantBytecode = BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, indexOfNumberToPrintInPool);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, constantBytecode, Bytecode);

    Bytecode printBytecode = BYTECODE(OP_PRINT);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, printBytecode, Bytecode);

    initVM(&vm, compiledCode);

    // Redirect stdout to a buffer
    char buffer[128];
    memset(buffer, 0, sizeof(buffer));
    fflush(stdout);
    FILE* prev_stdout = stdout;
    stdout = fmemopen(buffer, sizeof(buffer), "w");

    // Run the VM
    run(&vm);

    // Restore stdout
    fflush(stdout);
    fclose(stdout);
    stdout = prev_stdout;

    // Assertions
    // Check if the buffer contains the expected output
    char expectedOutput[128];
    sprintf(expectedOutput, "%f\n", numberToPrint.as.number);
    ASSERT(strcmp(buffer, expectedOutput) == 0);

    // Clean up
    _freeCompiledCode(&compiledCode);

    return SUCCESS_RETURN_CODE;
}

int test_vm_set_and_get_global() {
    CompiledCode compiledCode = newCompiledCode();

    // Setup a simple bytecode program: set global variable 'x' to 42, then get 'x'
    Constant valName = STRING_CONST("x");
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, valName, Constant);
    size_t valNameIndex = 0;
    Constant numberValue = DOUBLE_CONST(42.0);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, numberValue, Constant);
    size_t numberValueIndex = 1;

    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, numberValueIndex), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_DEFINE_GLOBAL_VAL, valNameIndex), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAL, valNameIndex), Bytecode);

    // Initialize VM with the bytecode
    VM vm;
    initVM(&vm, compiledCode);

    // Run the VM
    run(&vm);

    // Check the result on the stack should be 42
    Value expected_result = DOUBLE_VAL(42.0);
    Value actual_result = popVmStack(&vm);
    ASSERT(compareValues(actual_result, expected_result));

    // Clean up
    _freeCompiledCode(&compiledCode);

    return SUCCESS_RETURN_CODE;
}

int test_vm_binary_equal() {
    CompiledCode compiledCode = newCompiledCode();

    // Test case: 1 == 1; should push true
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, DOUBLE_CONST(1.0), Constant);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE(OP_BINARY_EQUAL), Bytecode);

    // Test case: 1 != 1; should push false
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE(OP_BINARY_NOT_EQUAL), Bytecode);

    VM vm;
    initVM(&vm, compiledCode);
    run(&vm);

    // Check results
    Value result_not_equal = popVmStack(&vm);  // Should be false
    Value result_equal = popVmStack(&vm);      // Should be true

    ASSERT(result_equal.type == TYPE_BOOLEAN && result_equal.as.booleanVal == true);
    ASSERT(result_not_equal.type == TYPE_BOOLEAN && result_not_equal.as.booleanVal == false);

    // Clean up
    _freeCompiledCode(&compiledCode);

    return SUCCESS_RETURN_CODE;
}

int test_vm_comparison_operations() {
    CompiledCode compiledCode = newCompiledCode();

    // Preparing constants: 1.0 and 2.0 for comparison tests
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, DOUBLE_CONST(1.0), Constant);  // Index 0
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, DOUBLE_CONST(2.0), Constant);  // Index 1

    // Test case: 1 < 2; should push true
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 1), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE(OP_BINARY_LT), Bytecode);

    // Test case: 1 <= 2; should push true
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 1), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE(OP_BINARY_LTE), Bytecode);

    // Test case: 2 > 1; should push true
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 1), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE(OP_BINARY_GT), Bytecode);

    // Test case: 2 >= 1; should push true
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 1), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE(OP_BINARY_GTE), Bytecode);

    VM vm;
    initVM(&vm, compiledCode);
    run(&vm);

    // Check results
    Value result_gte = popVmStack(&vm);
    Value result_gt = popVmStack(&vm);
    Value result_lte = popVmStack(&vm);
    Value result_lt = popVmStack(&vm);

    ASSERT(result_lt.type == TYPE_BOOLEAN && result_lt.as.booleanVal == true);
    ASSERT(result_lte.type == TYPE_BOOLEAN && result_lte.as.booleanVal == true);
    ASSERT(result_gt.type == TYPE_BOOLEAN && result_gt.as.booleanVal == true);
    ASSERT(result_gte.type == TYPE_BOOLEAN && result_gte.as.booleanVal == true);

    // Clean up
    _freeCompiledCode(&compiledCode);

    return SUCCESS_RETURN_CODE;
}

int test_vm_logical_operations() {
    CompiledCode compiledCode = newCompiledCode();

    // Preparing constants: true (1.0 as true) and false (0.0 as false) for logical tests
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, DOUBLE_CONST(1.0), Constant);  // Index 0 as true
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, DOUBLE_CONST(0.0), Constant);  // Index 1 as false

    // Test case: true && false; should push false
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);  // true
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 1), Bytecode);  // false
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE(OP_BINARY_LOGICAL_AND), Bytecode);

    // Test case: false || true; should push true
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 1), Bytecode);  // false
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);  // true
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE(OP_BINARY_LOGICAL_OR), Bytecode);

    VM vm;
    initVM(&vm, compiledCode);
    run(&vm);

    // Check results
    Value result_or = popVmStack(&vm);   // Should be true
    Value result_and = popVmStack(&vm);  // Should be false

    ASSERT(result_and.type == TYPE_BOOLEAN && result_and.as.booleanVal == false);
    ASSERT(result_or.type == TYPE_BOOLEAN && result_or.as.booleanVal == true);

    // Clean up
    _freeCompiledCode(&compiledCode);

    return SUCCESS_RETURN_CODE;
}

int test_vm_boolean_truthiness() {
    CompiledCode compiledCode = newCompiledCode();

    // Preparing constants
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, DOUBLE_CONST(0.0), Constant);  // Index 0

    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);  // Pushes 0.0
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE(OP_FALSE), Bytecode);                       // Pushes false
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE(OP_BINARY_LOGICAL_OR), Bytecode);           // Should evaluate to false

    VM vm;
    initVM(&vm, compiledCode);
    run(&vm);

    // Check result
    Value result = popVmStack(&vm);  // Should be false

    ASSERT(result.type == TYPE_BOOLEAN && result.as.booleanVal == false);

    // Clean up
    _freeCompiledCode(&compiledCode);

    return SUCCESS_RETURN_CODE;
}

int test_vm_unary_negation() {
    CompiledCode compiledCode = newCompiledCode();

    // Insert constant - The number to be negated
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, DOUBLE_CONST(5.0), Constant);  // Index 0, value 5.0

    // Load constant onto stack
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE(OP_UNARY_NEGATE), Bytecode);

    // Initialize VM and run the code
    VM vm;
    initVM(&vm, compiledCode);
    run(&vm);

    // Pop result from stack and check if it is the negated value
    Value result = popVmStack(&vm);

    // Assert the result is as expected (-5.0)
    ASSERT(result.type == TYPE_DOUBLE && result.as.doubleVal == -5.0);

    // Clean up
    _freeCompiledCode(&compiledCode);

    return SUCCESS_RETURN_CODE;
}

int test_vm_unary_not() {
    CompiledCode codeTrue = newCompiledCode(), codeFalse = newCompiledCode();

    // Test case for true: NOT true -> should push false
    INSERT_ARRAY(codeTrue.topLevelCodeObject.bytecodeArray, BYTECODE(OP_TRUE), Bytecode);
    INSERT_ARRAY(codeTrue.topLevelCodeObject.bytecodeArray, BYTECODE(OP_UNARY_NOT), Bytecode);

    // Test case for false: NOT false -> should push true
    INSERT_ARRAY(codeFalse.topLevelCodeObject.bytecodeArray, BYTECODE(OP_FALSE), Bytecode);
    INSERT_ARRAY(codeFalse.topLevelCodeObject.bytecodeArray, BYTECODE(OP_UNARY_NOT), Bytecode);

    // Initialize VM and run the code for true
    VM vmTrue;
    initVM(&vmTrue, codeTrue);
    run(&vmTrue);

    // Pop result from stack and check if it is false
    Value resultTrue = popVmStack(&vmTrue);  // Should be false
    ASSERT(resultTrue.type == TYPE_BOOLEAN && resultTrue.as.booleanVal == false);

    // Initialize VM and run the code for false
    VM vmFalse;
    initVM(&vmFalse, codeFalse);
    run(&vmFalse);

    // Pop result from stack and check if it is true
    Value resultFalse = popVmStack(&vmFalse);  // Should be true
    ASSERT(resultFalse.type == TYPE_BOOLEAN && resultFalse.as.booleanVal == true);

    // Clean up
    _freeCompiledCode(&codeTrue);
    _freeCompiledCode(&codeFalse);

    return SUCCESS_RETURN_CODE;
}

int test_vm_print_string_literal() {
    // Setup the VM and bytecode for a simple program that prints "Hello, World!"
    CompiledCode compiledCode = newCompiledCode();

    // Add the string to the constant pool
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, STRING_CONST("Hello, World!"), Constant);  // Index 0

    // Bytecode operations for loading a constant and printing
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE(OP_PRINT), Bytecode);

    // Initialize VM with the bytecode
    VM vm;
    initVM(&vm, compiledCode);

    CAPTURE_PRINT_OUTPUT({ run(&vm); }, { ASSERT(strcmp(buffer, "Hello, World!\n") == 0); });

    // Cleanup
    _freeCompiledCode(&compiledCode);

    return SUCCESS_RETURN_CODE;
}

int test_vm_simple_block_statement_and_cleanup() {
    // Setup bytecode for a block statement: {val a = 2; print a;}
    CompiledCode compiledCode = newCompiledCode();

    // Insert constants for "2" and "a"
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, DOUBLE_CONST(2.0), Constant);  // Index 0

    // Setup bytecode: Load constant 2, set global "a", get global "a", print
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE(OP_DEFINE_LOCAL_VAL_FAST), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_GET_LOCAL_VAL_FAST, 0), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE(OP_PRINT), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_POPN, 1), Bytecode);

    // Initialize VM
    VM vm;
    initVM(&vm, compiledCode);

    CAPTURE_PRINT_OUTPUT({ run(&vm); }, { ASSERT(strcmp(buffer, "2.000000\n") == 0); });

    // Check if the stack size is zero after execution
    ASSERT(vm.currFrame->SP == vm.stack);

    // Cleanup
    _freeCompiledCode(&compiledCode);

    return SUCCESS_RETURN_CODE;
}

int test_vm_nested_blocks_with_global_and_local_vars() {
    CompiledCode compiledCode = newCompiledCode();

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

    // Constants for the test
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, DOUBLE_CONST(100.0), Constant);  // Index 0
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, STRING_CONST("g"), Constant);    // Index 1
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, DOUBLE_CONST(10.0), Constant);   // Index 2
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, DOUBLE_CONST(20.0), Constant);   // Index 3
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, DOUBLE_CONST(30.0), Constant);   // Index 4

    // Bytecode
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);      // Load 100.0
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_DEFINE_GLOBAL_VAL, 1), Bytecode);  // Set g = 100.0
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAL, 1), Bytecode);     // Get g
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE(OP_PRINT), Bytecode);                           // Print g (100.0)

    // Start of block
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAL, 1), Bytecode);  // Get g (100.0 again)
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE(OP_PRINT), Bytecode);                        // Print g

    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 2), Bytecode);       // Load 10.0
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE(OP_DEFINE_LOCAL_VAL_FAST), Bytecode);            // Set x = 10.0
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_GET_LOCAL_VAL_FAST, 0), Bytecode);  // Load x
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE(OP_PRINT), Bytecode);                            // Print x

    // Nested block
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 3), Bytecode);       // Load 20.0 (shadowing g)
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE(OP_DEFINE_LOCAL_VAL_FAST), Bytecode);            // Set g = 20.0
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_GET_LOCAL_VAL_FAST, 1), Bytecode);  // Load g
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE(OP_PRINT), Bytecode);                            // Print g

    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 4), Bytecode);       // Load 30.0
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE(OP_DEFINE_LOCAL_VAL_FAST), Bytecode);            // Set y = 30.0
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_GET_LOCAL_VAL_FAST, 2), Bytecode);  // Load y
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE(OP_PRINT), Bytecode);                            // Print y

    // End of nested block
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_POPN, 2), Bytecode);

    // End of block
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_POPN, 1), Bytecode);

    // Initialize VM with the bytecode
    VM vm;
    initVM(&vm, compiledCode);

    // Run the test and capture output
    CAPTURE_PRINT_OUTPUT({ run(&vm); }, {
        char expectedOutput[] = "100.000000\n100.000000\n10.000000\n20.000000\n30.000000\n";
        ASSERT(strcmp(buffer, expectedOutput) == 0); });

    // Clean up
    _freeCompiledCode(&compiledCode);

    return SUCCESS_RETURN_CODE;
}

int test_vm_nested_if_statements() {
    // Initialize the VM and bytecode for the nested if statement example
    VM vm;
    CompiledCode compiledCode = newCompiledCode();

    // Add string literals to the constant pool
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, STRING_CONST("true-outer"), Constant);   // Index 0
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, STRING_CONST("true-inner"), Constant);   // Index 1
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, STRING_CONST("false-inner"), Constant);  // Index 2
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, STRING_CONST("false-outer"), Constant);  // Index 3

    ADD_BYTECODES(
        &compiledCode.topLevelCodeObject.bytecodeArray,
        BYTECODE(OP_TRUE),                         // Condition for outer if
        BYTECODE_OPERAND_1(OP_JUMP_IF_FALSE, 15),  // Jump to else block of outer if
        BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0),
        BYTECODE(OP_PRINT),
        BYTECODE(OP_FALSE),                        // Condition for inner if
        BYTECODE_OPERAND_1(OP_JUMP_IF_FALSE, 10),  // Jump to else block of inner if
        BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 1),
        BYTECODE(OP_PRINT),
        BYTECODE_OPERAND_1(OP_POPN, 0),
        BYTECODE_OPERAND_1(OP_JUMP, 13),  // Jump to end of inner if
        BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 2),
        BYTECODE(OP_PRINT),
        BYTECODE_OPERAND_1(OP_POPN, 0),
        BYTECODE_OPERAND_1(OP_POPN, 0),
        BYTECODE_OPERAND_1(OP_JUMP, 18),  // Jump to end of outer if
        BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 3),
        BYTECODE(OP_PRINT),
        BYTECODE_OPERAND_1(OP_POPN, 0));

    // Initialize VM with the bytecode
    initVM(&vm, compiledCode);

    // Run the VM and capture the output to verify correct execution
    CAPTURE_PRINT_OUTPUT({ run(&vm); }, {
        // Your assertions here, such as checking the output matches expected string sequence
        ASSERT(strcmp(buffer, "true-outer\nfalse-inner\n") == 0); });

    // Clean up
    _freeCompiledCode(&compiledCode);

    return SUCCESS_RETURN_CODE;  // Assuming SUCCESS_RETURN_CODE is defined as part of your testing framework
}

int test_vm_simple_block_expression() {
    // val a = { 10; };
    // print a;
    CompiledCode compiledCode = newCompiledCode();

    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, DOUBLE_CONST(10), Constant);   // Index 0
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, STRING_CONST("a"), Constant);  // Index 1

    // Bytecode
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_SWAP, 0), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_POPN, 0), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_DEFINE_GLOBAL_VAL, 1), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAL, 1), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE(OP_PRINT), Bytecode);

    VM vm;
    initVM(&vm, compiledCode);

    CAPTURE_PRINT_OUTPUT({ run(&vm); }, { ASSERT(strcmp(buffer, "10.000000\n") == 0); });

    _freeCompiledCode(&compiledCode);

    return SUCCESS_RETURN_CODE;
}

int test_vm_block_expression_with_statements() {
    // val a = { val b = 5; b + 3; };
    // print a;
    CompiledCode compiledCode = newCompiledCode();

    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, DOUBLE_CONST(5), Constant);    // Index 0
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, DOUBLE_CONST(3), Constant);    // Index 1
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, STRING_CONST("a"), Constant);  // Index 2

    // Bytecode
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE(OP_DEFINE_LOCAL_VAL_FAST), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_GET_LOCAL_VAL_FAST, 0), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 1), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE(OP_BINARY_ADD), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_SWAP, 1), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_POPN, 1), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_DEFINE_GLOBAL_VAL, 2), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAL, 2), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE(OP_PRINT), Bytecode);

    VM vm;
    initVM(&vm, compiledCode);

    CAPTURE_PRINT_OUTPUT({ run(&vm); }, { ASSERT(strcmp(buffer, "8.000000\n") == 0); });

    _freeCompiledCode(&compiledCode);

    return SUCCESS_RETURN_CODE;
}

int test_vm_block_expression_as_if_condition() {
    // if ({ val a = 1; a > 0; }) { print "Passed"; }
    CompiledCode compiledCode = newCompiledCode();

    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, DOUBLE_CONST(1), Constant);         // Index 0
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, DOUBLE_CONST(0), Constant);         // Index 1
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, STRING_CONST("Passed"), Constant);  // Index 2

    // Bytecode for the block expression
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE(OP_DEFINE_LOCAL_VAL_FAST), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_GET_LOCAL_VAL_FAST, 0), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 1), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE(OP_BINARY_GT), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_SWAP, 1), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_POPN, 1), Bytecode);

    // Bytecode for the if statement
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_JUMP_IF_FALSE, 11), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 2), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE(OP_PRINT), Bytecode);
    INSERT_ARRAY(compiledCode.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_POPN, 0), Bytecode);

    VM vm;
    initVM(&vm, compiledCode);

    CAPTURE_PRINT_OUTPUT({ run(&vm); }, { ASSERT(strcmp(buffer, "Passed\n") == 0); });

    _freeCompiledCode(&compiledCode);

    return SUCCESS_RETURN_CODE;
}

// Test simple var declaration and assignment in global scope
int test_vm_var_declaration_and_assignment_global() {
    CompiledCode compiledCode = newCompiledCode();

    // var a;
    // a = 10;
    // print a;
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, IDENTIFIER_CONST("a"), Constant);  // Index 0
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, DOUBLE_CONST(10), Constant);       // Index 1

    ADD_BYTECODES(&compiledCode.topLevelCodeObject.bytecodeArray,
                  BYTECODE(OP_NULL),
                  BYTECODE_OPERAND_1(OP_DEFINE_GLOBAL_VAR, 0),
                  BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 1),
                  BYTECODE_OPERAND_1(OP_SET_GLOBAL_VAR, 0),
                  BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAR, 0),
                  BYTECODE(OP_PRINT));

    VM vm;
    initVM(&vm, compiledCode);
    CAPTURE_PRINT_OUTPUT({ run(&vm); }, { ASSERT(strcmp(buffer, "10.000000\n") == 0); });

    _freeCompiledCode(&compiledCode);

    return SUCCESS_RETURN_CODE;
}

// Test simple var declaration and assignment in local scope
int test_vm_var_declaration_and_assignment_local() {
    CompiledCode compiledCode = newCompiledCode();

    // {
    //   var a;
    //   a = 10;
    //   print a;
    // }
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, DOUBLE_CONST(10), Constant);  // Index 0

    ADD_BYTECODES(&compiledCode.topLevelCodeObject.bytecodeArray,
                  BYTECODE(OP_NULL),
                  BYTECODE(OP_DEFINE_LOCAL_VAR_FAST),
                  BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0),
                  BYTECODE_OPERAND_1(OP_SET_LOCAL_VAR_FAST, 0),
                  BYTECODE_OPERAND_1(OP_GET_LOCAL_VAR_FAST, 0),
                  BYTECODE(OP_PRINT),
                  BYTECODE_OPERAND_1(OP_POPN, 1));

    VM vm;
    initVM(&vm, compiledCode);
    CAPTURE_PRINT_OUTPUT({ run(&vm); }, { ASSERT(strcmp(buffer, "10.000000\n") == 0); });

    _freeCompiledCode(&compiledCode);

    return SUCCESS_RETURN_CODE;
}

// Test block expression with val declarations
int test_vm_block_expression_with_val_declarations() {
    CompiledCode compiledCode = newCompiledCode();

    // val a = { val c = 1; c + 2; }
    // val b = { val c = 10; c + 2; }
    // print a;
    // print b;
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, DOUBLE_CONST(1), Constant);        // Index 0
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, DOUBLE_CONST(2), Constant);        // Index 1
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, DOUBLE_CONST(10), Constant);       // Index 2
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, IDENTIFIER_CONST("a"), Constant);  // Index 3
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, IDENTIFIER_CONST("b"), Constant);  // Index 4

    ADD_BYTECODES(&compiledCode.topLevelCodeObject.bytecodeArray,
                  // Block for 'a'
                  BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0),
                  BYTECODE(OP_DEFINE_LOCAL_VAL_FAST),
                  BYTECODE_OPERAND_1(OP_GET_LOCAL_VAL_FAST, 0),
                  BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 1),
                  BYTECODE(OP_BINARY_ADD),
                  BYTECODE_OPERAND_1(OP_SWAP, 1),
                  BYTECODE_OPERAND_1(OP_POPN, 1),
                  // End of block for 'a'
                  BYTECODE_OPERAND_1(OP_DEFINE_GLOBAL_VAL, 3),
                  // Block for 'b'
                  BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 2),
                  BYTECODE(OP_DEFINE_LOCAL_VAL_FAST),
                  BYTECODE_OPERAND_1(OP_GET_LOCAL_VAL_FAST, 0),
                  BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 1),
                  BYTECODE(OP_BINARY_ADD),
                  BYTECODE_OPERAND_1(OP_SWAP, 1),
                  BYTECODE_OPERAND_1(OP_POPN, 1),
                  // End of block for 'b'
                  BYTECODE_OPERAND_1(OP_DEFINE_GLOBAL_VAL, 4),
                  // Print 'a' and 'b'
                  BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAL, 3),
                  BYTECODE(OP_PRINT),
                  BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAL, 4),
                  BYTECODE(OP_PRINT));

    VM vm;
    initVM(&vm, compiledCode);
    CAPTURE_PRINT_OUTPUT({ run(&vm); }, { ASSERT(strcmp(buffer, "3.000000\n12.000000\n") == 0); });

    _freeCompiledCode(&compiledCode);

    return SUCCESS_RETURN_CODE;
}

// Test var and val declarations in nested blocks
int test_vm_var_val_declarations_in_nested_blocks() {
    CompiledCode compiledCode = newCompiledCode();

    // {
    //   var a = 1;
    //   {
    //     val b = 2;
    //     print a;
    //     print b;
    //   }
    //   print a;
    // }
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, DOUBLE_CONST(1), Constant);  // Index 0
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, DOUBLE_CONST(2), Constant);  // Index 1

    ADD_BYTECODES(&compiledCode.topLevelCodeObject.bytecodeArray,
                  // Outer block
                  BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0),
                  BYTECODE(OP_DEFINE_LOCAL_VAR_FAST),
                  // Inner block
                  BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 1),
                  BYTECODE(OP_DEFINE_LOCAL_VAL_FAST),
                  BYTECODE_OPERAND_1(OP_GET_LOCAL_VAR_FAST, 0),
                  BYTECODE(OP_PRINT),
                  BYTECODE_OPERAND_1(OP_GET_LOCAL_VAL_FAST, 1),
                  BYTECODE(OP_PRINT),
                  BYTECODE_OPERAND_1(OP_POPN, 1),
                  // End of inner block
                  BYTECODE_OPERAND_1(OP_GET_LOCAL_VAR_FAST, 0),
                  BYTECODE(OP_PRINT),
                  BYTECODE_OPERAND_1(OP_POPN, 1));
    // End of outer block

    VM vm;
    initVM(&vm, compiledCode);
    CAPTURE_PRINT_OUTPUT({ run(&vm); }, { ASSERT(strcmp(buffer, "1.000000\n2.000000\n1.000000\n") == 0); });

    _freeCompiledCode(&compiledCode);

    return SUCCESS_RETURN_CODE;
}

// Test assignment to var in global and local scopes
int test_vm_var_assignment_global_and_local() {
    CompiledCode compiledCode = newCompiledCode();

    // var a;
    // a = 1;
    // print a;
    // {
    //   var b;
    //   b = 2;
    //   print b;
    // }
    // print a;
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, IDENTIFIER_CONST("a"), Constant);  // Index 0
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, DOUBLE_CONST(1), Constant);        // Index 1
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, DOUBLE_CONST(2), Constant);        // Index 2

    ADD_BYTECODES(&compiledCode.topLevelCodeObject.bytecodeArray,
                  // Global var 'a'
                  BYTECODE(OP_NULL),
                  BYTECODE_OPERAND_1(OP_DEFINE_GLOBAL_VAR, 0),
                  BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 1),
                  BYTECODE_OPERAND_1(OP_SET_GLOBAL_VAR, 0),
                  BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAR, 0),
                  BYTECODE(OP_PRINT),
                  // Local var 'b'
                  BYTECODE(OP_NULL),
                  BYTECODE(OP_DEFINE_LOCAL_VAR_FAST),
                  BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 2),
                  BYTECODE_OPERAND_1(OP_SET_LOCAL_VAR_FAST, 0),
                  BYTECODE_OPERAND_1(OP_GET_LOCAL_VAL_FAST, 0),
                  BYTECODE(OP_PRINT),
                  BYTECODE_OPERAND_1(OP_POPN, 1),
                  // Print global var 'a' again
                  BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAR, 0),
                  BYTECODE(OP_PRINT));

    VM vm;
    initVM(&vm, compiledCode);
    CAPTURE_PRINT_OUTPUT({ run(&vm); }, { ASSERT(strcmp(buffer, "1.000000\n2.000000\n1.000000\n") == 0); });

    _freeCompiledCode(&compiledCode);

    return SUCCESS_RETURN_CODE;
}

int test_vm_global_declaration_and_local_assignment() {
    CompiledCode compiledCode = newCompiledCode();

    // var a;
    // {
    //   a = 1;
    //   print a;
    // }
    // print a;
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, IDENTIFIER_CONST("a"), Constant);  // Index 0
    INSERT_ARRAY(compiledCode.topLevelCodeObject.constantPool, DOUBLE_CONST(1), Constant);        // Index 1

    ADD_BYTECODES(&compiledCode.topLevelCodeObject.bytecodeArray,
                  // Global var declaration
                  BYTECODE(OP_NULL),
                  BYTECODE_OPERAND_1(OP_DEFINE_GLOBAL_VAR, 0),
                  // Local block
                  BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 1),
                  BYTECODE_OPERAND_1(OP_SET_GLOBAL_VAR, 0),
                  BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAR, 0),
                  BYTECODE(OP_PRINT),
                  BYTECODE_OPERAND_1(OP_POPN, 0),
                  // Print global var 'a' again
                  BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAR, 0),
                  BYTECODE(OP_PRINT));

    VM vm;
    initVM(&vm, compiledCode);
    CAPTURE_PRINT_OUTPUT({ run(&vm); }, { ASSERT(strcmp(buffer, "1.000000\n1.000000\n") == 0); });

    _freeCompiledCode(&compiledCode);

    return SUCCESS_RETURN_CODE;
}

int test_vm_iteration_statement_simple() {
    // while (true) print "Loop";
    Source testSource = {
        .rootStatements = {
            ITERATION_STATEMENT(
                PRIMARY_EXPRESSION(BOOLEAN_LITERAL(true)),
                PRINT_STATEMENT(PRIMARY_EXPRESSION(STRING_LITERAL("\"Loop\""))))},
        .numberOfStatements = 1,
    };

    COMPILE_TEST_SOURCE

    VM vm;
    initVM(&vm, compiledCode);

    // Redirect stdout to a buffer
    char buffer[256];
    memset(buffer, 0, sizeof(buffer));
    FILE* originalStdout = stdout;
    stdout = fmemopen(buffer, sizeof(buffer), "w");

    // Run the VM for a limited number of instructions to avoid infinite loop
    for (int i = 0; i < 26; i++) {
        step(&vm);
    }

    // Restore stdout
    fflush(stdout);
    fclose(stdout);
    stdout = originalStdout;

    // Check the output
    ASSERT(strncmp(buffer, "Loop\nLoop\nLoop\nLoop\nLoop\n", 25) == 0);

    FREE_COMPILER;

    return SUCCESS_RETURN_CODE;
}

int test_vm_iteration_statement_with_variable() {
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

    VM vm;
    initVM(&vm, compiledCode);

    // Redirect stdout to a buffer
    char buffer[256];
    memset(buffer, 0, sizeof(buffer));
    FILE* originalStdout = stdout;
    stdout = fmemopen(buffer, sizeof(buffer), "w");

    run(&vm);

    // Restore stdout
    fflush(stdout);
    fclose(stdout);
    stdout = originalStdout;

    // Check the output
    ASSERT(strcmp(buffer, "0.000000\n1.000000\n2.000000\n") == 0);

    FREE_COMPILER;

    return SUCCESS_RETURN_CODE;
}

int test_vm_nested_iteration_statements() {
    // var i = 0; while (i < 3) { var j = 0; while (j < 2) { print i + j; j = j + 1; } i = i + 1; }
    Source testSource = {
        .rootStatements = {
            VAR_DECLARATION_STATEMENT("i", PRIMARY_EXPRESSION(NUMBER_LITERAL("0"))),
            ITERATION_STATEMENT(
                COMPARISON_EXPRESSION(
                    PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("i")),
                    PRIMARY_EXPRESSION(NUMBER_LITERAL("3")),
                    TOKEN(TOKEN_LESSER)),
                BLOCK_STATEMENT(
                    VAR_DECLARATION_STATEMENT("j", PRIMARY_EXPRESSION(NUMBER_LITERAL("0"))),
                    ITERATION_STATEMENT(
                        COMPARISON_EXPRESSION(
                            PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("j")),
                            PRIMARY_EXPRESSION(NUMBER_LITERAL("2")),
                            TOKEN(TOKEN_LESSER)),
                        BLOCK_STATEMENT(
                            PRINT_STATEMENT(
                                ADDITIVE_EXPRESSION(
                                    PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("i")),
                                    TOKEN(TOKEN_PLUS),
                                    PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("j")))),
                            ASSIGNMENT_STATEMENT(
                                PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("j")),
                                ADDITIVE_EXPRESSION(
                                    PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("j")),
                                    TOKEN(TOKEN_PLUS),
                                    PRIMARY_EXPRESSION(NUMBER_LITERAL("1")))))),
                    ASSIGNMENT_STATEMENT(
                        PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("i")),
                        ADDITIVE_EXPRESSION(
                            PRIMARY_EXPRESSION(IDENTIFIER_LITERAL("i")),
                            TOKEN(TOKEN_PLUS),
                            PRIMARY_EXPRESSION(NUMBER_LITERAL("1"))))))},
        .numberOfStatements = 2,
    };

    COMPILE_TEST_SOURCE
    VM vm;
    initVM(&vm, compiledCode);

    CAPTURE_PRINT_OUTPUT({ run(&vm); }, { ASSERT(strcmp(buffer, "0.000000\n1.000000\n1.000000\n2.000000\n2.000000\n3.000000\n") == 0); });

    FREE_COMPILER;
    return SUCCESS_RETURN_CODE;
}

int test_vm_lambda_no_params() {
    CompiledCode code = newCompiledCode();

    // Create lambda: lambda () { 42; }
    Function* lambda = malloc(sizeof(Function));
    lambda->parameterCount = 0;
    lambda->code = malloc(sizeof(CompiledCodeObject));
    INIT_ARRAY(lambda->code->bytecodeArray, Bytecode);
    INIT_ARRAY(lambda->code->constantPool, Constant);

    // Lambda body: return 42
    INSERT_ARRAY(lambda->code->constantPool, DOUBLE_CONST(42), Constant);
    INSERT_ARRAY(lambda->code->bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);
    INSERT_ARRAY(lambda->code->bytecodeArray, BYTECODE_OPERAND_1(OP_POPN, 0), Bytecode);
    INSERT_ARRAY(lambda->code->bytecodeArray, BYTECODE(OP_RETURN), Bytecode);

    // Main code
    INSERT_ARRAY(code.topLevelCodeObject.constantPool, LAMBDA_CONST(lambda), Constant);
    INSERT_ARRAY(code.topLevelCodeObject.constantPool, IDENTIFIER_CONST("getFortyTwo"), Constant);
    INSERT_ARRAY(code.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LAMBDA, 0), Bytecode);
    INSERT_ARRAY(code.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_DEFINE_GLOBAL_VAL, 1), Bytecode);
    INSERT_ARRAY(code.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAL, 1), Bytecode);
    INSERT_ARRAY(code.topLevelCodeObject.bytecodeArray, BYTECODE(OP_CALL), Bytecode);
    INSERT_ARRAY(code.topLevelCodeObject.bytecodeArray, BYTECODE(OP_PRINT), Bytecode);

    VM vm;
    initVM(&vm, code);

    CAPTURE_PRINT_OUTPUT({ run(&vm); }, { ASSERT(strcmp(buffer, "42.000000\n") == 0); });

    freeVMButNotCompiledCode(&vm);
    _freeCompiledCode(&code);
    freeLambdaFunction(lambda);

    return SUCCESS_RETURN_CODE;
}

int test_vm_lambda_one_param() {
    CompiledCode code = newCompiledCode();

    // Create lambda: lambda (x) { x; }
    Function* lambda = malloc(sizeof(Function));
    lambda->parameterCount = 1;
    lambda->code = malloc(sizeof(CompiledCodeObject));
    INIT_ARRAY(lambda->code->bytecodeArray, Bytecode);
    INIT_ARRAY(lambda->code->constantPool, Constant);

    // Lambda body: return x
    INSERT_ARRAY(lambda->code->bytecodeArray, BYTECODE_OPERAND_1(OP_GET_LOCAL_VAR_FAST, 0), Bytecode);
    INSERT_ARRAY(lambda->code->bytecodeArray, BYTECODE_OPERAND_1(OP_POPN, 0), Bytecode);
    INSERT_ARRAY(lambda->code->bytecodeArray, BYTECODE(OP_RETURN), Bytecode);

    // Main code
    INSERT_ARRAY(code.topLevelCodeObject.constantPool, LAMBDA_CONST(lambda), Constant);
    INSERT_ARRAY(code.topLevelCodeObject.constantPool, IDENTIFIER_CONST("identity"), Constant);
    INSERT_ARRAY(code.topLevelCodeObject.constantPool, DOUBLE_CONST(5), Constant);
    INSERT_ARRAY(code.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LAMBDA, 0), Bytecode);
    INSERT_ARRAY(code.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_DEFINE_GLOBAL_VAL, 1), Bytecode);
    INSERT_ARRAY(code.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 2), Bytecode);
    INSERT_ARRAY(code.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAL, 1), Bytecode);
    INSERT_ARRAY(code.topLevelCodeObject.bytecodeArray, BYTECODE(OP_CALL), Bytecode);
    INSERT_ARRAY(code.topLevelCodeObject.bytecodeArray, BYTECODE(OP_PRINT), Bytecode);

    VM vm;
    initVM(&vm, code);

    CAPTURE_PRINT_OUTPUT({ run(&vm); }, { ASSERT(strcmp(buffer, "5.000000\n") == 0); });

    freeVMButNotCompiledCode(&vm);
    _freeCompiledCode(&code);
    freeLambdaFunction(lambda);
    return SUCCESS_RETURN_CODE;
}

int test_vm_lambda_two_params() {
    CompiledCode code = newCompiledCode();

    // Create lambda: lambda (a, b) { a + b; }
    Function* lambda = malloc(sizeof(Function));
    lambda->parameterCount = 2;
    lambda->code = malloc(sizeof(CompiledCodeObject));
    INIT_ARRAY(lambda->code->bytecodeArray, Bytecode);
    INIT_ARRAY(lambda->code->constantPool, Constant);

    // Lambda body: a + b
    INSERT_ARRAY(lambda->code->bytecodeArray, BYTECODE_OPERAND_1(OP_GET_LOCAL_VAR_FAST, 0), Bytecode);
    INSERT_ARRAY(lambda->code->bytecodeArray, BYTECODE_OPERAND_1(OP_GET_LOCAL_VAR_FAST, 1), Bytecode);
    INSERT_ARRAY(lambda->code->bytecodeArray, BYTECODE(OP_BINARY_ADD), Bytecode);
    INSERT_ARRAY(lambda->code->bytecodeArray, BYTECODE_OPERAND_1(OP_POPN, 0), Bytecode);
    INSERT_ARRAY(lambda->code->bytecodeArray, BYTECODE(OP_RETURN), Bytecode);

    // Main code
    INSERT_ARRAY(code.topLevelCodeObject.constantPool, LAMBDA_CONST(lambda), Constant);
    INSERT_ARRAY(code.topLevelCodeObject.constantPool, IDENTIFIER_CONST("add"), Constant);
    INSERT_ARRAY(code.topLevelCodeObject.constantPool, DOUBLE_CONST(3), Constant);
    INSERT_ARRAY(code.topLevelCodeObject.constantPool, DOUBLE_CONST(4), Constant);
    INSERT_ARRAY(code.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LAMBDA, 0), Bytecode);
    INSERT_ARRAY(code.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_DEFINE_GLOBAL_VAL, 1), Bytecode);
    INSERT_ARRAY(code.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 2), Bytecode);
    INSERT_ARRAY(code.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 3), Bytecode);
    INSERT_ARRAY(code.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAL, 1), Bytecode);
    INSERT_ARRAY(code.topLevelCodeObject.bytecodeArray, BYTECODE(OP_CALL), Bytecode);
    INSERT_ARRAY(code.topLevelCodeObject.bytecodeArray, BYTECODE(OP_PRINT), Bytecode);

    VM vm;
    initVM(&vm, code);

    CAPTURE_PRINT_OUTPUT({ run(&vm); }, { ASSERT(strcmp(buffer, "7.000000\n") == 0); });

    freeVMButNotCompiledCode(&vm);
    _freeCompiledCode(&code);
    freeLambdaFunction(lambda);
    return SUCCESS_RETURN_CODE;
}

int test_vm_lambda_nested_calls() {
    CompiledCode code = newCompiledCode();

    // Create lambda: lambda (a, b) { a * b; }
    Function* multiplyLambda = malloc(sizeof(Function));
    multiplyLambda->parameterCount = 2;
    multiplyLambda->code = malloc(sizeof(CompiledCodeObject));
    INIT_ARRAY(multiplyLambda->code->bytecodeArray, Bytecode);
    INIT_ARRAY(multiplyLambda->code->constantPool, Constant);

    INSERT_ARRAY(multiplyLambda->code->bytecodeArray, BYTECODE_OPERAND_1(OP_GET_LOCAL_VAR_FAST, 0), Bytecode);
    INSERT_ARRAY(multiplyLambda->code->bytecodeArray, BYTECODE_OPERAND_1(OP_GET_LOCAL_VAR_FAST, 1), Bytecode);
    INSERT_ARRAY(multiplyLambda->code->bytecodeArray, BYTECODE(OP_BINARY_MULTIPLY), Bytecode);
    INSERT_ARRAY(multiplyLambda->code->bytecodeArray, BYTECODE_OPERAND_1(OP_POPN, 0), Bytecode);
    INSERT_ARRAY(multiplyLambda->code->bytecodeArray, BYTECODE(OP_RETURN), Bytecode);

    // Create lambda: lambda (a, b) { a + b; }
    Function* addLambda = malloc(sizeof(Function));
    addLambda->parameterCount = 2;
    addLambda->code = malloc(sizeof(CompiledCodeObject));
    INIT_ARRAY(addLambda->code->bytecodeArray, Bytecode);
    INIT_ARRAY(addLambda->code->constantPool, Constant);

    INSERT_ARRAY(addLambda->code->bytecodeArray, BYTECODE_OPERAND_1(OP_GET_LOCAL_VAR_FAST, 0), Bytecode);
    INSERT_ARRAY(addLambda->code->bytecodeArray, BYTECODE_OPERAND_1(OP_GET_LOCAL_VAR_FAST, 1), Bytecode);
    INSERT_ARRAY(addLambda->code->bytecodeArray, BYTECODE(OP_BINARY_ADD), Bytecode);
    INSERT_ARRAY(addLambda->code->bytecodeArray, BYTECODE_OPERAND_1(OP_POPN, 0), Bytecode);
    INSERT_ARRAY(addLambda->code->bytecodeArray, BYTECODE(OP_RETURN), Bytecode);

    // Main code
    INSERT_ARRAY(code.topLevelCodeObject.constantPool, LAMBDA_CONST(multiplyLambda), Constant);
    INSERT_ARRAY(code.topLevelCodeObject.constantPool, IDENTIFIER_CONST("multiply"), Constant);
    INSERT_ARRAY(code.topLevelCodeObject.constantPool, LAMBDA_CONST(addLambda), Constant);
    INSERT_ARRAY(code.topLevelCodeObject.constantPool, IDENTIFIER_CONST("add"), Constant);
    INSERT_ARRAY(code.topLevelCodeObject.constantPool, DOUBLE_CONST(2), Constant);
    INSERT_ARRAY(code.topLevelCodeObject.constantPool, DOUBLE_CONST(3), Constant);
    INSERT_ARRAY(code.topLevelCodeObject.constantPool, DOUBLE_CONST(4), Constant);
    INSERT_ARRAY(code.topLevelCodeObject.constantPool, DOUBLE_CONST(5), Constant);

    INSERT_ARRAY(code.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LAMBDA, 0), Bytecode);
    INSERT_ARRAY(code.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_DEFINE_GLOBAL_VAL, 1), Bytecode);
    INSERT_ARRAY(code.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LAMBDA, 2), Bytecode);
    INSERT_ARRAY(code.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_DEFINE_GLOBAL_VAL, 3), Bytecode);
    INSERT_ARRAY(code.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 4), Bytecode);
    INSERT_ARRAY(code.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 5), Bytecode);
    INSERT_ARRAY(code.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAL, 1), Bytecode);
    INSERT_ARRAY(code.topLevelCodeObject.bytecodeArray, BYTECODE(OP_CALL), Bytecode);
    INSERT_ARRAY(code.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 6), Bytecode);
    INSERT_ARRAY(code.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 7), Bytecode);
    INSERT_ARRAY(code.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAL, 1), Bytecode);
    INSERT_ARRAY(code.topLevelCodeObject.bytecodeArray, BYTECODE(OP_CALL), Bytecode);
    INSERT_ARRAY(code.topLevelCodeObject.bytecodeArray, BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAL, 3), Bytecode);
    INSERT_ARRAY(code.topLevelCodeObject.bytecodeArray, BYTECODE(OP_CALL), Bytecode);
    INSERT_ARRAY(code.topLevelCodeObject.bytecodeArray, BYTECODE(OP_PRINT), Bytecode);

    VM vm;
    initVM(&vm, code);

    CAPTURE_PRINT_OUTPUT({ run(&vm); }, { ASSERT(strcmp(buffer, "26.000000\n") == 0); });

    freeVMButNotCompiledCode(&vm);
    _freeCompiledCode(&code);
    freeLambdaFunction(multiplyLambda);
    freeLambdaFunction(addLambda);
    return SUCCESS_RETURN_CODE;
}

int test_vm_simple_struct() {
    CompiledCode code = newCompiledCode();

    // Create a simple struct with two fields
    INSERT_ARRAY(code.topLevelCodeObject.constantPool, STRING_CONST("x"), Constant);
    INSERT_ARRAY(code.topLevelCodeObject.constantPool, DOUBLE_CONST(10), Constant);
    INSERT_ARRAY(code.topLevelCodeObject.constantPool, STRING_CONST("y"), Constant);
    INSERT_ARRAY(code.topLevelCodeObject.constantPool, STRING_CONST("hello"), Constant);
    INSERT_ARRAY(code.topLevelCodeObject.constantPool, STRING_CONST("myStruct"), Constant);

    ADD_BYTECODES(&code.topLevelCodeObject.bytecodeArray,
                  BYTECODE(OP_NEW_STRUCT),
                  BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 1),      // 10
                  BYTECODE_OPERAND_1(OP_SET_FIELD, 0),          // "x"
                  BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 3),      // "hello"
                  BYTECODE_OPERAND_1(OP_SET_FIELD, 2),          // "y"
                  BYTECODE_OPERAND_1(OP_DEFINE_GLOBAL_VAL, 4),  // Store struct in global "myStruct"
                  BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAL, 4),     // Get "myStruct"
                  BYTECODE_OPERAND_1(OP_GET_FIELD, 0),          // Get "x"
                  BYTECODE(OP_PRINT),
                  BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAL, 4),  // Get "myStruct" again
                  BYTECODE_OPERAND_1(OP_GET_FIELD, 2),       // Get "y"
                  BYTECODE(OP_PRINT));

    VM vm;
    initVM(&vm, code);

    CAPTURE_PRINT_OUTPUT({ run(&vm); }, { ASSERT(strcmp(buffer, "10.000000\nhello\n") == 0); });

    freeVMButNotCompiledCode(&vm);
    _freeCompiledCode(&code);
    return SUCCESS_RETURN_CODE;
}

int test_vm_nested_structs() {
    CompiledCode code = newCompiledCode();

    // Create a nested struct
    INSERT_ARRAY(code.topLevelCodeObject.constantPool, STRING_CONST("outer"), Constant);
    INSERT_ARRAY(code.topLevelCodeObject.constantPool, STRING_CONST("inner"), Constant);
    INSERT_ARRAY(code.topLevelCodeObject.constantPool, STRING_CONST("value"), Constant);
    INSERT_ARRAY(code.topLevelCodeObject.constantPool, DOUBLE_CONST(42), Constant);

    ADD_BYTECODES(&code.topLevelCodeObject.bytecodeArray,
                  BYTECODE(OP_NEW_STRUCT),                      // Outer struct
                  BYTECODE(OP_NEW_STRUCT),                      // Inner struct
                  BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 3),      // 42
                  BYTECODE_OPERAND_1(OP_SET_FIELD, 2),          // "value" in inner struct
                  BYTECODE_OPERAND_1(OP_SET_FIELD, 1),          // "inner" in outer struct
                  BYTECODE_OPERAND_1(OP_DEFINE_GLOBAL_VAL, 0),  // Store outer struct in global "outer"
                  BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAL, 0),     // Get "outer"
                  BYTECODE_OPERAND_1(OP_GET_FIELD, 1),          // Get "inner"
                  BYTECODE_OPERAND_1(OP_GET_FIELD, 2),          // Get "value" from inner
                  BYTECODE(OP_PRINT));

    VM vm;
    initVM(&vm, code);

    CAPTURE_PRINT_OUTPUT({ run(&vm); }, { ASSERT(strcmp(buffer, "42.000000\n") == 0); });

    freeVMButNotCompiledCode(&vm);
    _freeCompiledCode(&code);
    return SUCCESS_RETURN_CODE;
}

int test_vm_struct_field_assignment() {
    CompiledCode code = newCompiledCode();

    INSERT_ARRAY(code.topLevelCodeObject.constantPool, STRING_CONST("myStruct"), Constant);
    INSERT_ARRAY(code.topLevelCodeObject.constantPool, STRING_CONST("field"), Constant);
    INSERT_ARRAY(code.topLevelCodeObject.constantPool, DOUBLE_CONST(10), Constant);
    INSERT_ARRAY(code.topLevelCodeObject.constantPool, DOUBLE_CONST(20), Constant);

    ADD_BYTECODES(&code.topLevelCodeObject.bytecodeArray,
                  BYTECODE(OP_NEW_STRUCT),
                  BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 2),      // 10
                  BYTECODE_OPERAND_1(OP_SET_FIELD, 1),          // "field"
                  BYTECODE_OPERAND_1(OP_DEFINE_GLOBAL_VAL, 0),  // "myStruct"
                  BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAL, 0),     // Get "myStruct"
                  BYTECODE_OPERAND_1(OP_GET_FIELD, 1),          // Get "field"
                  BYTECODE(OP_PRINT),
                  BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAL, 0),  // Get "myStruct"
                  BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 3),   // 20
                  BYTECODE_OPERAND_1(OP_SET_FIELD, 1),       // Set "field" to 20
                  BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAL, 0),  // Get "myStruct"
                  BYTECODE_OPERAND_1(OP_GET_FIELD, 1),       // Get "field"
                  BYTECODE(OP_PRINT));

    VM vm;
    initVM(&vm, code);

    CAPTURE_PRINT_OUTPUT({ run(&vm); }, { ASSERT(strcmp(buffer, "10.000000\n20.000000\n") == 0); });

    freeVMButNotCompiledCode(&vm);
    _freeCompiledCode(&code);
    return SUCCESS_RETURN_CODE;
}

int test_vm_struct_in_function_call() {
    CompiledCode code = newCompiledCode();

    // Create a function that takes a struct and returns its "x" field
    Function* getX = malloc(sizeof(Function));
    getX->parameterCount = 1;
    getX->code = malloc(sizeof(CompiledCodeObject));
    INIT_ARRAY(getX->code->bytecodeArray, Bytecode);
    INIT_ARRAY(getX->code->constantPool, Constant);

    INSERT_ARRAY(getX->code->constantPool, STRING_CONST("x"), Constant);

    ADD_BYTECODES(&getX->code->bytecodeArray,
                  BYTECODE_OPERAND_1(OP_GET_LOCAL_VAR_FAST, 0),  // Get the struct parameter
                  BYTECODE_OPERAND_1(OP_GET_FIELD, 0),           // Get "x" field
                  BYTECODE(OP_RETURN));

    // Main code
    INSERT_ARRAY(code.topLevelCodeObject.constantPool, LAMBDA_CONST(getX), Constant);
    INSERT_ARRAY(code.topLevelCodeObject.constantPool, STRING_CONST("getX"), Constant);
    INSERT_ARRAY(code.topLevelCodeObject.constantPool, STRING_CONST("x"), Constant);
    INSERT_ARRAY(code.topLevelCodeObject.constantPool, DOUBLE_CONST(42), Constant);
    INSERT_ARRAY(code.topLevelCodeObject.constantPool, STRING_CONST("myStruct"), Constant);

    ADD_BYTECODES(&code.topLevelCodeObject.bytecodeArray,
                  BYTECODE_OPERAND_1(OP_LAMBDA, 0),             // Create getX function
                  BYTECODE_OPERAND_1(OP_DEFINE_GLOBAL_VAL, 1),  // Define "getX"
                  BYTECODE(OP_NEW_STRUCT),                      // Create struct
                  BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 3),      // 42
                  BYTECODE_OPERAND_1(OP_SET_FIELD, 2),          // Set "x" to 42
                  BYTECODE_OPERAND_1(OP_DEFINE_GLOBAL_VAL, 4),  // Store struct in global "myStruct"
                  BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAL, 4),     // Get "myStruct" for function call
                  BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAL, 1),     // Get "getX" function
                  BYTECODE(OP_CALL),                            // Call getX(struct)
                  BYTECODE(OP_PRINT)                            // Print result
    );

    VM vm;
    initVM(&vm, code);

    CAPTURE_PRINT_OUTPUT({ run(&vm); }, { ASSERT(strcmp(buffer, "42.000000\n") == 0); });

    freeVMButNotCompiledCode(&vm);
    _freeCompiledCode(&code);
    freeLambdaFunction(getX);
    return SUCCESS_RETURN_CODE;
}