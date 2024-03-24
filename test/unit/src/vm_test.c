#include "vm.h"

#include <stdio.h>
#include <stdlib.h>

#include "../minunit.h"
#include "../utils.h"
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
    vm->SP--;
    return *(vm->SP);
}

int test_vm_addition() {
    CompiledCode code = (CompiledCode){
        .constantPool = (ConstantPool){},
        .bytecodeArray = (BytecodeArray){}};
    INIT_ARRAY(code.bytecodeArray, Bytecode);
    INIT_ARRAY(code.constantPool, Constant);

    // Create a simple bytecode program: 1 + 2
    INSERT_ARRAY(code.constantPool, DOUBLE_CONST(1.0), Constant);  // put Constant in index 0
    INSERT_ARRAY(code.constantPool, DOUBLE_CONST(2.0), Constant);  // put Constant in index 1
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 1), Bytecode);
    INSERT_ARRAY(code.bytecodeArray, BYTECODE(OP_BINARY_ADD), Bytecode);

    // Initialize VM with the bytecode
    VM vm;
    initVM(&vm, code);

    // Run the VM
    run(&vm);

    // Check the result on the stack
    Value expected_result = DOUBLE_VAL(3.0);
    Value actual_result = popVmStack(&vm);
    ASSERT(compareValues(actual_result, expected_result));

    // Clean up
    FREE_ARRAY(code.bytecodeArray);
    FREE_ARRAY(code.constantPool);

    return SUCCESS_RETURN_CODE;
}

int test_vm_print() {
    // Initialize the VM
    VM vm;
    CompiledCode code = (CompiledCode){
        .constantPool = (ConstantPool){},
        .bytecodeArray = (BytecodeArray){}};
    INIT_ARRAY(code.bytecodeArray, Bytecode);
    INIT_ARRAY(code.constantPool, Value);

    // Setup bytecode for a print statement
    Constant numberToPrint = DOUBLE_CONST(42);
    INSERT_ARRAY(code.constantPool, numberToPrint, Constant);
    size_t indexOfNumberToPrintInPool = 0;

    Bytecode constantBytecode = BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, indexOfNumberToPrintInPool);
    INSERT_ARRAY(code.bytecodeArray, constantBytecode, Bytecode);

    Bytecode printBytecode = BYTECODE(OP_PRINT);
    INSERT_ARRAY(code.bytecodeArray, printBytecode, Bytecode);

    initVM(&vm, code);

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
    FREE_ARRAY(code.bytecodeArray);
    FREE_ARRAY(code.constantPool);

    return SUCCESS_RETURN_CODE;
}

int test_vm_set_and_get_global() {
    CompiledCode code = (CompiledCode){
        .constantPool = (ConstantPool){},
        .bytecodeArray = (BytecodeArray){}};
    INIT_ARRAY(code.bytecodeArray, Bytecode);
    INIT_ARRAY(code.constantPool, Constant);

    // Setup a simple bytecode program: set global variable 'x' to 42, then get 'x'
    Constant valName = STRING_CONST("x");
    INSERT_ARRAY(code.constantPool, valName, Constant);
    size_t valNameIndex = 0;
    Constant numberValue = DOUBLE_CONST(42.0);
    INSERT_ARRAY(code.constantPool, numberValue, Constant);
    size_t numberValueIndex = 1;

    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, numberValueIndex), Bytecode);
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_SET_GLOBAL_VAL, valNameIndex), Bytecode);
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAL, valNameIndex), Bytecode);

    // Initialize VM with the bytecode
    VM vm;
    initVM(&vm, code);

    // Run the VM
    run(&vm);

    // Check the result on the stack should be 42
    Value expected_result = DOUBLE_VAL(42.0);
    Value actual_result = popVmStack(&vm);
    ASSERT(compareValues(actual_result, expected_result));

    // Clean up
    FREE_ARRAY(code.bytecodeArray);
    FREE_ARRAY(code.constantPool);

    return SUCCESS_RETURN_CODE;
}

int test_vm_binary_equal() {
    CompiledCode code = (CompiledCode){
        .constantPool = (ConstantPool){},
        .bytecodeArray = (BytecodeArray){}};
    INIT_ARRAY(code.bytecodeArray, Bytecode);
    INIT_ARRAY(code.constantPool, Constant);

    // Test case: 1 == 1; should push true
    INSERT_ARRAY(code.constantPool, DOUBLE_CONST(1.0), Constant);
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);
    INSERT_ARRAY(code.bytecodeArray, BYTECODE(OP_BINARY_EQUAL), Bytecode);

    // Test case: 1 != 1; should push false
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);
    INSERT_ARRAY(code.bytecodeArray, BYTECODE(OP_BINARY_NOT_EQUAL), Bytecode);

    VM vm;
    initVM(&vm, code);
    run(&vm);

    // Check results
    Value result_not_equal = popVmStack(&vm);  // Should be false
    Value result_equal = popVmStack(&vm);      // Should be true

    ASSERT(result_equal.type == TYPE_BOOLEAN && result_equal.as.booleanVal == true);
    ASSERT(result_not_equal.type == TYPE_BOOLEAN && result_not_equal.as.booleanVal == false);

    // Clean up
    FREE_ARRAY(code.bytecodeArray);
    FREE_ARRAY(code.constantPool);

    return SUCCESS_RETURN_CODE;
}

int test_vm_comparison_operations() {
    CompiledCode code = (CompiledCode){
        .constantPool = (ConstantPool){},
        .bytecodeArray = (BytecodeArray){}};
    INIT_ARRAY(code.bytecodeArray, Bytecode);
    INIT_ARRAY(code.constantPool, Constant);

    // Preparing constants: 1.0 and 2.0 for comparison tests
    INSERT_ARRAY(code.constantPool, DOUBLE_CONST(1.0), Constant);  // Index 0
    INSERT_ARRAY(code.constantPool, DOUBLE_CONST(2.0), Constant);  // Index 1

    // Test case: 1 < 2; should push true
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 1), Bytecode);
    INSERT_ARRAY(code.bytecodeArray, BYTECODE(OP_BINARY_LT), Bytecode);

    // Test case: 1 <= 2; should push true
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 1), Bytecode);
    INSERT_ARRAY(code.bytecodeArray, BYTECODE(OP_BINARY_LTE), Bytecode);

    // Test case: 2 > 1; should push true
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 1), Bytecode);
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);
    INSERT_ARRAY(code.bytecodeArray, BYTECODE(OP_BINARY_GT), Bytecode);

    // Test case: 2 >= 1; should push true
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 1), Bytecode);
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);
    INSERT_ARRAY(code.bytecodeArray, BYTECODE(OP_BINARY_GTE), Bytecode);

    VM vm;
    initVM(&vm, code);
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
    FREE_ARRAY(code.bytecodeArray);
    FREE_ARRAY(code.constantPool);

    return SUCCESS_RETURN_CODE;
}

int test_vm_logical_operations() {
    CompiledCode code = (CompiledCode){
        .constantPool = (ConstantPool){},
        .bytecodeArray = (BytecodeArray){}};
    INIT_ARRAY(code.bytecodeArray, Bytecode);
    INIT_ARRAY(code.constantPool, Constant);

    // Preparing constants: true (1.0 as true) and false (0.0 as false) for logical tests
    INSERT_ARRAY(code.constantPool, DOUBLE_CONST(1.0), Constant);  // Index 0 as true
    INSERT_ARRAY(code.constantPool, DOUBLE_CONST(0.0), Constant);  // Index 1 as false

    // Test case: true && false; should push false
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);  // true
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 1), Bytecode);  // false
    INSERT_ARRAY(code.bytecodeArray, BYTECODE(OP_BINARY_LOGICAL_AND), Bytecode);

    // Test case: false || true; should push true
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 1), Bytecode);  // false
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);  // true
    INSERT_ARRAY(code.bytecodeArray, BYTECODE(OP_BINARY_LOGICAL_OR), Bytecode);

    VM vm;
    initVM(&vm, code);
    run(&vm);

    // Check results
    Value result_or = popVmStack(&vm);   // Should be true
    Value result_and = popVmStack(&vm);  // Should be false

    ASSERT(result_and.type == TYPE_BOOLEAN && result_and.as.booleanVal == false);
    ASSERT(result_or.type == TYPE_BOOLEAN && result_or.as.booleanVal == true);

    // Clean up
    FREE_ARRAY(code.bytecodeArray);
    FREE_ARRAY(code.constantPool);

    return SUCCESS_RETURN_CODE;
}

int test_vm_boolean_truthiness() {
    CompiledCode code = (CompiledCode){
        .constantPool = (ConstantPool){},
        .bytecodeArray = (BytecodeArray){}};
    INIT_ARRAY(code.bytecodeArray, Bytecode);
    INIT_ARRAY(code.constantPool, Constant);

    // Preparing constants
    INSERT_ARRAY(code.constantPool, DOUBLE_CONST(0.0), Constant);  // Index 0

    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);  // Pushes 0.0
    INSERT_ARRAY(code.bytecodeArray, BYTECODE(OP_FALSE), Bytecode);                       // Pushes false
    INSERT_ARRAY(code.bytecodeArray, BYTECODE(OP_BINARY_LOGICAL_OR), Bytecode);           // Should evaluate to false

    VM vm;
    initVM(&vm, code);
    run(&vm);

    // Check result
    Value result = popVmStack(&vm);  // Should be false

    ASSERT(result.type == TYPE_BOOLEAN && result.as.booleanVal == false);

    // Clean up
    FREE_ARRAY(code.bytecodeArray);
    FREE_ARRAY(code.constantPool);

    return SUCCESS_RETURN_CODE;
}

int test_vm_unary_negation() {
    CompiledCode code = {0};
    INIT_ARRAY(code.bytecodeArray, Bytecode);
    INIT_ARRAY(code.constantPool, Constant);

    // Insert constant - The number to be negated
    INSERT_ARRAY(code.constantPool, DOUBLE_CONST(5.0), Constant);  // Index 0, value 5.0

    // Load constant onto stack
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);
    INSERT_ARRAY(code.bytecodeArray, BYTECODE(OP_UNARY_NEGATE), Bytecode);

    // Initialize VM and run the code
    VM vm;
    initVM(&vm, code);
    run(&vm);

    // Pop result from stack and check if it is the negated value
    Value result = popVmStack(&vm);

    // Assert the result is as expected (-5.0)
    ASSERT(result.type == TYPE_DOUBLE && result.as.doubleVal == -5.0);

    // Clean up
    FREE_ARRAY(code.bytecodeArray);
    FREE_ARRAY(code.constantPool);

    return SUCCESS_RETURN_CODE;
}

int test_vm_unary_not() {
    CompiledCode codeTrue = {0}, codeFalse = {0};
    INIT_ARRAY(codeTrue.bytecodeArray, Bytecode);
    INIT_ARRAY(codeFalse.bytecodeArray, Bytecode);

    // Test case for true: NOT true -> should push false
    INSERT_ARRAY(codeTrue.bytecodeArray, BYTECODE(OP_TRUE), Bytecode);
    INSERT_ARRAY(codeTrue.bytecodeArray, BYTECODE(OP_UNARY_NOT), Bytecode);

    // Test case for false: NOT false -> should push true
    INSERT_ARRAY(codeFalse.bytecodeArray, BYTECODE(OP_FALSE), Bytecode);
    INSERT_ARRAY(codeFalse.bytecodeArray, BYTECODE(OP_UNARY_NOT), Bytecode);

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
    FREE_ARRAY(codeTrue.bytecodeArray);
    FREE_ARRAY(codeFalse.bytecodeArray);

    return SUCCESS_RETURN_CODE;
}

int test_vm_print_string_literal() {
    // Setup the VM and bytecode for a simple program that prints "Hello, World!"
    CompiledCode code = {.constantPool = {}, .bytecodeArray = {}};
    INIT_ARRAY(code.bytecodeArray, Bytecode);
    INIT_ARRAY(code.constantPool, Constant);

    // Add the string to the constant pool
    INSERT_ARRAY(code.constantPool, STRING_CONST("Hello, World!"), Constant);  // Index 0

    // Bytecode operations for loading a constant and printing
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);
    INSERT_ARRAY(code.bytecodeArray, BYTECODE(OP_PRINT), Bytecode);

    // Initialize VM with the bytecode
    VM vm;
    initVM(&vm, code);

    CAPTURE_PRINT_OUTPUT({ run(&vm); }, { ASSERT(strcmp(buffer, "Hello, World!\n") == 0); });

    // Cleanup
    FREE_ARRAY(code.bytecodeArray);
    FREE_ARRAY(code.constantPool);

    return SUCCESS_RETURN_CODE;
}

int test_vm_simple_block_statement_and_cleanup() {
    // Setup bytecode for a block statement: {val a = 2; print a;}
    CompiledCode code = {.constantPool = {}, .bytecodeArray = {}};
    INIT_ARRAY(code.bytecodeArray, Bytecode);
    INIT_ARRAY(code.constantPool, Constant);

    // Insert constants for "2" and "a"
    INSERT_ARRAY(code.constantPool, DOUBLE_CONST(2.0), Constant);  // Index 0

    // Setup bytecode: Load constant 2, set global "a", get global "a", print
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);
    INSERT_ARRAY(code.bytecodeArray, BYTECODE(OP_SET_LOCAL_VAL_FAST), Bytecode);
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_GET_LOCAL_VAL_FAST, 0), Bytecode);
    INSERT_ARRAY(code.bytecodeArray, BYTECODE(OP_PRINT), Bytecode);
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_POPN, 1), Bytecode);

    // Initialize VM
    VM vm;
    initVM(&vm, code);

    CAPTURE_PRINT_OUTPUT({ run(&vm); }, { ASSERT(strcmp(buffer, "2.000000\n") == 0); });

    // Check if the stack size is zero after execution
    ASSERT(vm.SP == vm.stack);

    // Cleanup
    FREE_ARRAY(code.bytecodeArray);
    FREE_ARRAY(code.constantPool);

    return SUCCESS_RETURN_CODE;
}

int test_vm_nested_blocks_with_global_and_local_vars() {
    CompiledCode code = {.constantPool = {}, .bytecodeArray = {}};
    INIT_ARRAY(code.bytecodeArray, Bytecode);
    INIT_ARRAY(code.constantPool, Constant);

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
    INSERT_ARRAY(code.constantPool, DOUBLE_CONST(100.0), Constant);  // Index 0
    INSERT_ARRAY(code.constantPool, STRING_CONST("g"), Constant);    // Index 1
    INSERT_ARRAY(code.constantPool, DOUBLE_CONST(10.0), Constant);   // Index 2
    INSERT_ARRAY(code.constantPool, DOUBLE_CONST(20.0), Constant);   // Index 3
    INSERT_ARRAY(code.constantPool, DOUBLE_CONST(30.0), Constant);   // Index 4

    // Bytecode
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 0), Bytecode);   // Load 100.0
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_SET_GLOBAL_VAL, 1), Bytecode);  // Set g = 100.0
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAL, 1), Bytecode);  // Get g
    INSERT_ARRAY(code.bytecodeArray, BYTECODE(OP_PRINT), Bytecode);                        // Print g (100.0)

    // Start of block
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAL, 1), Bytecode);  // Get g (100.0 again)
    INSERT_ARRAY(code.bytecodeArray, BYTECODE(OP_PRINT), Bytecode);                        // Print g

    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 2), Bytecode);       // Load 10.0
    INSERT_ARRAY(code.bytecodeArray, BYTECODE(OP_SET_LOCAL_VAL_FAST), Bytecode);               // Set x = 10.0
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_GET_LOCAL_VAL_FAST, 0), Bytecode);  // Load x
    INSERT_ARRAY(code.bytecodeArray, BYTECODE(OP_PRINT), Bytecode);                            // Print x

    // Nested block
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 3), Bytecode);       // Load 20.0 (shadowing g)
    INSERT_ARRAY(code.bytecodeArray, BYTECODE(OP_SET_LOCAL_VAL_FAST), Bytecode);               // Set g = 20.0
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_GET_LOCAL_VAL_FAST, 1), Bytecode);  // Load g
    INSERT_ARRAY(code.bytecodeArray, BYTECODE(OP_PRINT), Bytecode);                            // Print g

    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, 4), Bytecode);       // Load 30.0
    INSERT_ARRAY(code.bytecodeArray, BYTECODE(OP_SET_LOCAL_VAL_FAST), Bytecode);               // Set y = 30.0
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_GET_LOCAL_VAL_FAST, 2), Bytecode);  // Load y
    INSERT_ARRAY(code.bytecodeArray, BYTECODE(OP_PRINT), Bytecode);                            // Print y

    // End of nested block
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_POPN, 2), Bytecode);

    // End of block
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_OPERAND_1(OP_POPN, 1), Bytecode);

    // Initialize VM with the bytecode
    VM vm;
    initVM(&vm, code);

    // Run the test and capture output
    CAPTURE_PRINT_OUTPUT({ run(&vm); }, {
        char expectedOutput[] = "100.000000\n100.000000\n10.000000\n20.000000\n30.000000\n";
        ASSERT(strcmp(buffer, expectedOutput) == 0); });

    // Clean up
    FREE_ARRAY(code.bytecodeArray);
    FREE_ARRAY(code.constantPool);

    return SUCCESS_RETURN_CODE;
}

int test_vm_nested_if_statements() {
    // Initialize the VM and bytecode for the nested if statement example
    VM vm;
    CompiledCode code = {
        .constantPool = (ConstantPool){},
        .bytecodeArray = (BytecodeArray){}};
    INIT_ARRAY(code.bytecodeArray, Bytecode);
    INIT_ARRAY(code.constantPool, Constant);

    // Add string literals to the constant pool
    INSERT_ARRAY(code.constantPool, STRING_CONST("true-outer"), Constant);   // Index 0
    INSERT_ARRAY(code.constantPool, STRING_CONST("true-inner"), Constant);   // Index 1
    INSERT_ARRAY(code.constantPool, STRING_CONST("false-inner"), Constant);  // Index 2
    INSERT_ARRAY(code.constantPool, STRING_CONST("false-outer"), Constant);  // Index 3

    ADD_BYTECODES(
        &code.bytecodeArray,
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
    initVM(&vm, code);

    // Run the VM and capture the output to verify correct execution
    CAPTURE_PRINT_OUTPUT({ run(&vm); }, {
        // Your assertions here, such as checking the output matches expected string sequence
        ASSERT(strcmp(buffer, "true-outer\nfalse-inner\n") == 0); });

    // Clean up
    FREE_ARRAY(code.bytecodeArray);
    FREE_ARRAY(code.constantPool);

    return SUCCESS_RETURN_CODE;  // Assuming SUCCESS_RETURN_CODE is defined as part of your testing framework
}
