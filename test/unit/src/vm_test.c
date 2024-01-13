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

int test_vm_addition() {
    // Create a simple bytecode program: 1 + 2
    BytecodeArray bytecode;
    INIT_ARRAY(bytecode, Bytecode);

    INSERT_ARRAY(bytecode, BYTECODE_CONSTANT_DOUBLE(1.0), Bytecode);
    INSERT_ARRAY(bytecode, BYTECODE_CONSTANT_DOUBLE(2.0), Bytecode);
    INSERT_ARRAY(bytecode, BYTECODE(OP_ADD), Bytecode);

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

int test_vm_print() {
    // Initialize the VM
    VM vm;
    BytecodeArray bytecodeArray;
    INIT_ARRAY(bytecodeArray, Bytecode);

    // Setup bytecode for a print statement
    double numberToPrint = 42.0;
    Bytecode constantBytecode = BYTECODE_CONSTANT_DOUBLE(numberToPrint);
    INSERT_ARRAY(bytecodeArray, constantBytecode, Bytecode);

    Bytecode printBytecode = BYTECODE(OP_PRINT);
    INSERT_ARRAY(bytecodeArray, printBytecode, Bytecode);

    initVM(&vm, &bytecodeArray);

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
    sprintf(expectedOutput, "%f\n", numberToPrint);
    ASSERT(strcmp(buffer, expectedOutput) == 0);

    // Clean up
    FREE_ARRAY(bytecodeArray);

    return SUCCESS_RETURN_CODE;
}