#include "vm.h"

#include "../minunit.h"
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

// Simplified pop function for testing
static Value popVmStack(VM* vm) {
    vm->SP--;
    return *(vm->SP);
}

// Test case 1: Execute addition in the VM
int test_vm_addition() {
    // Create a simple bytecode program: 1 + 2
    BytecodeArray bytecode;
    INIT_ARRAY(bytecode, Bytecode);

    INSERT_ARRAY(bytecode, BYTECODE_CONSTANT_DOUBLE(1.0), Bytecode);
    INSERT_ARRAY(bytecode, BYTECODE_CONSTANT_DOUBLE(2.0), Bytecode);
    INSERT_ARRAY(bytecode, BYTECODE_ADD(), Bytecode);

    // Initialize VM with the bytecode
    VM vm;
    initVM(&vm, &bytecode);

    // Run the VM
    run(&vm);

    // Check the result on the stack
    Value expected_result = DOUBLE_VAL(3.0);
    Value actual_result = popVmStack(&vm);
    ASSERT(compareValues(actual_result, expected_result));

    // Clean up
    FREE_ARRAY(bytecode);

    return SUCCESS_RETURN_CODE;
}