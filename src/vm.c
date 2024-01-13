#include "vm.h"

#include <stdio.h>
#include <stdlib.h>

#include "bytecode.h"
#include "value.h"

/**
 * Initialize VM with some source code.
 */
void initVM(VM* vm, BytecodeArray* bytecode) {
    vm->instructions = bytecode;
    vm->IP = vm->instructions->values;  // Set instruction pointer to the beginning of bytecode
    vm->SP = vm->stack;                 // Set stack pointer to the top of the stack

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

// Convert a BytecodeConstant (from the compiler) to a Value (which is used in the VM)
// TODO: make this a macro
static Value bytecodeConstantToValue(BytecodeConstant* bytecodeConstant) {
    switch (bytecodeConstant->type) {
        case TYPE_DOUBLE:
            return (Value){.type = TYPE_DOUBLE, .as = {.doubleVal = bytecodeConstant->as.doubleVal}};
            break;

        default:
            break;
    };
}

// Print a Value
static void printValue(Value value) {
    switch (value.type) {
        case NUMBER_LITERAL:
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
        case OP_CONSTANT:
            push(vm, bytecodeConstantToValue(&(instruction->operands.constant)));
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
    Bytecode* lastInstruction = &(vm->instructions->values[vm->instructions->used]);

    while (vm->IP != lastInstruction) {
        step(vm);
    }

    // TEMPORARY: print the last value on the stack
    Value value = *(vm->SP - 1);
    printValue(value);
    printf("\n");
}

void interpret(VM* vm, BytecodeArray* bytecode) {
    for (int i = 0; i < bytecode->used; i++) {
        vm->instructions->values[vm->instructions->used] = bytecode->values[i];
        vm->instructions->used++;
    }

    run(vm);
}