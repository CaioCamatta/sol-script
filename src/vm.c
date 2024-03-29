#include "vm.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include "bytecode.h"
#include "colors.h"
#include "config.h"
#include "debug.h"
#include "util/hash_table.h"
#include "value.h"

/**
 * Initialize VM with some source code.
 */
void initVM(VM* vm, CompiledCode compiledCode) {
    vm->compiledCode = compiledCode;
    vm->IP = vm->compiledCode.bytecodeArray.values;  // Set instruction pointer to the beginning of bytecode
    vm->SP = vm->stack;                              // Set stack pointer to the top of the stack
    initHashTable(&vm->globals);

    // Initialize stack with empty values.
    for (int i = 0; i < STACK_MAX; ++i) {
        vm->stack[i] = (Value){.type = TYPE_NULL};
    }
}

/* Print a runtime error and exit. */
static void runtimeError(VM* vm, const char* format, ...) {
    // Print error message
    va_list args;
    va_start(args, format);
    fprintf(stderr, KRED);
    vfprintf(stderr, format, args);
    fprintf(stderr, RESET);
    va_end(args);

    // TODO: make this not exit during a REPL session.
    exit(EXIT_FAILURE);
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

// Pop N values from the stack
void popN(VM* vm, int n) {
    if (n > (vm->SP - vm->stack)) {
        runtimeError(vm, "Runtime error: attempted to pop more elements from the stack than are in the stack.");
    }
    vm->SP -= n;
}

/**
 * peek(0) peeks the top of the stack
 * peek(1) peeks the second highest element of the stack
 */
static Value peek(VM* vm, int distance) {
    return vm->SP[-1 - distance];
}

// Convert a Constant in the constants pool from the compiled bytecode into a runtime Value
Value bytecodeConstantToValue(VM* vm, size_t constantIndex) {
    Constant constant = vm->compiledCode.constantPool.values[constantIndex];
    switch (constant.type) {
        case CONST_TYPE_DOUBLE:
            return (Value){.type = TYPE_DOUBLE, .as = {.doubleVal = constant.as.number}};
        case CONST_TYPE_STRING:
        case CONST_TYPE_IDENTIFIER:
            return (Value){.type = TYPE_STRING, .as = {.stringVal = constant.as.string}};
    }
}

// Print a Value
static void printValue(Value value) {
    switch (value.type) {
        case TYPE_DOUBLE:
            printf("%f", value.as.doubleVal);
            break;
        case TYPE_BOOLEAN:
            printf("%s", value.as.booleanVal ? "true" : "false");
            break;
        case TYPE_NULL:
            printf("null");
            break;
        case TYPE_STRING:
            printf("%s", value.as.stringVal);
            break;
    };
    printf("\n");
}

// Apply an operation to two doubles, push double to stack
#define BINARY_NUMBER_OP(operation)                                                  \
    do {                                                                             \
        if (!IS_DOUBLE(peek(vm, 0)) || !IS_DOUBLE(peek(vm, 1))) {                    \
            runtimeError(vm, "Operands of arithmetic expression must be numbers.");  \
        }                                                                            \
        Value operand2 = pop(vm);                                                    \
        Value operand1 = pop(vm);                                                    \
        push(vm, DOUBLE_VAL(operand1.as.doubleVal operation operand2.as.doubleVal)); \
    } while (false)

// Apply an operation to two doubles, push boolean to stack
#define BINARY_NUMBER_OP_TRUTHY(operation)                                                \
    do {                                                                                  \
        if (!IS_DOUBLE(peek(vm, 0)) || !IS_DOUBLE(peek(vm, 1))) {                         \
            runtimeError(vm, "Operands must be numbers.");                                \
        }                                                                                 \
        Value operand2 = pop(vm);                                                         \
        Value operand1 = pop(vm);                                                         \
        push(vm, BOOL_VAL((operand1.as.doubleVal operation operand2.as.doubleVal) == 1)); \
    } while (false)

/* Null, false, and the number 0 are falsey. Everything else is truthy */
static bool isFalsey(Value value) {
    return IS_NULL(value) || (IS_BOOLEAN(value) && !(value.as.booleanVal)) || (IS_DOUBLE(value) && (value.as.doubleVal == 0));
}

// Apply an operation to two booleans (converting if needed), push boolean to stack
#define BINARY_TRUTHY_OP(operation)                                            \
    do {                                                                       \
        Value operand2 = pop(vm);                                              \
        Value operand1 = pop(vm);                                              \
        push(vm, BOOL_VAL(!isFalsey(operand1) operation !isFalsey(operand2))); \
    } while (false)

/**
 * Execute one instruction in the VM.
 */
void step(VM* vm) {
    // Fetch the instruction at the instruction pointer
    Bytecode* instruction = vm->IP;

#if DEBUG_VM
    printf(KGRY "%-4ld " RESET, vm->IP - vm->compiledCode.bytecodeArray.values);
    printStack(vm->SP, &(vm->stack[0]));
#endif

    switch (instruction->type) {
        case OP_LOAD_CONSTANT:
            push(vm, bytecodeConstantToValue(vm, instruction->maybeOperand1));
            break;
        case OP_SET_GLOBAL_VAL: {
            Value value = pop(vm);
            size_t constantIndex = instruction->maybeOperand1;
            Constant constant = vm->compiledCode.constantPool.values[constantIndex];
            hashTableInsert(&vm->globals, constant.as.string, value);
            break;
        }
        case OP_GET_GLOBAL_VAL: {
            size_t constantIndex = instruction->maybeOperand1;
            Constant constant = vm->compiledCode.constantPool.values[constantIndex];
            Value value = hashTableGet(&vm->globals, constant.as.string)->value;
            push(vm, value);
            break;
        }
        case OP_SET_LOCAL_VAL_FAST:
            break;  // no action necessary to set locals. The compiler handles everything. 🤯
        case OP_GET_LOCAL_VAL_FAST: {
            size_t stackIndex = instruction->maybeOperand1;
            Value value = vm->stack[stackIndex];
            push(vm, value);
            break;
        }
        case OP_POPN: {
            size_t n = instruction->maybeOperand1;
            popN(vm, n);
            break;
        }
        case OP_PRINT: {
            Value value = pop(vm);
            printValue(value);
            break;
        }
        case OP_TRUE: {
            push(vm, BOOL_VAL(true));
            break;
        }
        case OP_FALSE: {
            push(vm, BOOL_VAL(false));
            break;
        }
        case OP_UNARY_NEGATE: {
            Value value = pop(vm);
            if (value.type == TYPE_DOUBLE) {
                push(vm, DOUBLE_VAL(-value.as.doubleVal));
            } else {
                runtimeError(vm, "Operand of unary negation must be a number.");
            }
            break;
        }
        case OP_UNARY_NOT: {
            Value value = pop(vm);
            if (value.type == TYPE_BOOLEAN) {
                push(vm, BOOL_VAL(!value.as.booleanVal));
            } else {
                runtimeError(vm, "Operand of unary not must be a boolean.");
            }
            break;
        }
        case OP_BINARY_ADD: {
            // TODO: Consider allowing adding strings together
            BINARY_NUMBER_OP(+);
            break;
        }
        case OP_BINARY_SUBTRACT: {
            BINARY_NUMBER_OP(-);
            break;
        }
        case OP_BINARY_MULTIPLY: {
            BINARY_NUMBER_OP(*);
            break;
        }
        case OP_BINARY_DIVIDE: {
            BINARY_NUMBER_OP(/);
            break;
        }
        case OP_BINARY_GT: {
            BINARY_NUMBER_OP_TRUTHY(>);
            break;
        }
        case OP_BINARY_GTE: {
            BINARY_NUMBER_OP_TRUTHY(>=);
            break;
        }
        case OP_BINARY_LT: {
            BINARY_NUMBER_OP_TRUTHY(<);
            break;
        }
        case OP_BINARY_LTE: {
            BINARY_NUMBER_OP_TRUTHY(<=);
            break;
        }
        case OP_BINARY_LOGICAL_AND: {
            BINARY_TRUTHY_OP(&&);
            break;
        }
        case OP_BINARY_LOGICAL_OR: {
            BINARY_TRUTHY_OP(||);
            break;
        }
        case OP_BINARY_EQUAL: {
            BINARY_TRUTHY_OP(==);
            break;
        }
        case OP_BINARY_NOT_EQUAL: {
            BINARY_TRUTHY_OP(!=);
            break;
        }
        case OP_JUMP_IF_FALSE: {
            Value value = pop(vm);
            // If condition is falsey, jump over the "then" branch
            if (isFalsey(value)) {
                size_t IPAfterThenBranch = instruction->maybeOperand1;
                // We have to subtract 1 because the instruction we jump to will be skipped (in `vm->IP++;` below).
                vm->IP = &(vm->compiledCode.bytecodeArray.values[IPAfterThenBranch - 1]);
            }
            break;
        }
        case OP_JUMP: {
            size_t IPToJumpTo = instruction->maybeOperand1;
            // We have to subtract 1 because the instruction we jump to will be skipped (in `vm->IP++;` below).
            vm->IP = &(vm->compiledCode.bytecodeArray.values[IPToJumpTo - 1]);
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

#if DEBUG_VM
    printf("Started executing VM.\n");
#endif

    while (vm->IP < lastInstruction) {
        step(vm);
    }
}