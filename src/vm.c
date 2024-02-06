#include "vm.h"

#include <stdio.h>
#include <stdlib.h>

#include "bytecode.h"
#include "value.h"

/**
 * Initialize VM with some source code.
 */
void initVM(VM* vm, CompiledCode compiledCode) {
    vm->compiledCode = compiledCode;
    vm->IP = vm->compiledCode.bytecodeArray.values;  // Set instruction pointer to the beginning of bytecode
    vm->SP = vm->stack;                              // Set stack pointer to the top of the stack

    // Initialize stack with empty values.
    for (int i = 0; i < STACK_MAX; ++i) {
        vm->stack[i] = (Value){.type = TYPE_NULL};
    }
}

// Push value onto the stack
void push(VM* vm, Value value) {
    *vm->SP = value;
    vm->SP++;
}

// Pop (retrieve) value from stack
Value pop(VM* vm) {
    vm->SP--;
    return *(vm->SP);
}

// Convert a Constant in the constants pool from the compiled bytecode into a runtime Value
Value bytecodeConstantToValue(VM* vm, size_t constantIndex) {
    Constant constant = vm->compiledCode.constantPool.values[constantIndex];
    switch (constant.type) {
        case CONST_TYPE_DOUBLE:
            return (Value){.type = TYPE_DOUBLE, .as = {.doubleVal = constant.as.number}};
        case CONST_TYPE_STRING:
            return (Value){.type = TYPE_STRING, .as = {.stringVal = constant.as.string}};
    }
}

// Print a Value
static void printValue(Value value) {
    switch (value.type) {
        case TYPE_DOUBLE:
            printf("%f", value.as.doubleVal);
            break;

        default:
            break;
    };
}

/**
 * Execute one instruction in the VM.
 */
void step(VM* vm) {
    // Fetch the instruction at the instruction pointer
    Bytecode* instruction = vm->IP;

    switch (instruction->type) {
        case OP_LOAD_CONSTANT:
            // constant 1 is assumed to exist
            push(vm, bytecodeConstantToValue(vm, instruction->maybeConstantIndex));
            break;
        case OP_ADD: {
            Value operand1 = pop(vm);
            Value operand2 = pop(vm);

            Value result = DOUBLE_VAL(operand1.as.doubleVal + operand2.as.doubleVal);
            push(vm, result);

            break;
        }
        case OP_PRINT: {
            Value value = pop(vm);
            printValue(value);

            break;
        }
        default:
            // Handle any unknown or unimplemented opcodes.
            fprintf(stderr, "Unimplemented opcode %d\n", instruction->type);
            exit(EXIT_FAILURE);
    }

    // Move the instruction pointer to the next instruction
    vm->IP++;
}

/**
 * Execute code in the VM until it halts.
 */
void run(VM* vm) {
    // Find the last instruction so we know when to stop executing
    Bytecode* lastInstruction = &(vm->compiledCode.bytecodeArray.values[vm->compiledCode.bytecodeArray.used]);

    while (vm->IP != lastInstruction) {
        step(vm);
    }

    printf("\n");
}