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
    CompiledCode code = (CompiledCode){
        .constantPool = (ConstantPool){},
        .bytecodeArray = (BytecodeArray){}};
    INIT_ARRAY(code.bytecodeArray, Bytecode);
    INIT_ARRAY(code.constantPool, Constant);

    // Create a simple bytecode program: 1 + 2
    INSERT_ARRAY(code.constantPool, DOUBLE_CONST(1.0), Constant);  // put Constant in index 0
    INSERT_ARRAY(code.constantPool, DOUBLE_CONST(2.0), Constant);  // put Constant in index 1
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_CONSTANT_1(OP_LOAD_CONSTANT, 0), Bytecode);
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_CONSTANT_1(OP_LOAD_CONSTANT, 1), Bytecode);
    INSERT_ARRAY(code.bytecodeArray, BYTECODE(OP_ADD), Bytecode);

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

    Bytecode constantBytecode = BYTECODE_CONSTANT_1(OP_LOAD_CONSTANT, indexOfNumberToPrintInPool);
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

int test_vm_setAndGetGlobal() {
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

    INSERT_ARRAY(code.bytecodeArray, BYTECODE_CONSTANT_1(OP_LOAD_CONSTANT, numberValueIndex), Bytecode);
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_CONSTANT_1(OP_SET_VAL, valNameIndex), Bytecode);
    INSERT_ARRAY(code.bytecodeArray, BYTECODE_CONSTANT_1(OP_GET_VAL, valNameIndex), Bytecode);

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