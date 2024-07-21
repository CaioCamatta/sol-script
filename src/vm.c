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
    initHashTable(&vm->globals);

    // Push the initial call frame
    CallFrame* frame = &vm->frames[0];
    // initVM accepts a struct, not a pointer, so we manually copy it to the heap.
    frame->codeObject = malloc(sizeof(CompiledCode));
    frame->codeObject->bytecodeArray = compiledCode.topLevelCodeObject.bytecodeArray;
    frame->codeObject->constantPool = compiledCode.topLevelCodeObject.constantPool;
    frame->parameterCount = 0;  // Top-level code is not a function
    frame->SP = vm->stack;      // Set stack pointer to the top of the stack
    frame->stackStart = vm->stack;
    frame->IP = frame->codeObject->bytecodeArray.values;

    vm->currFrame = frame;
}

void freeVM(VM* vm) {
    // Free the top level object we manually initialized in initVM;
    free(vm->frames[0].codeObject);
}

/* Print a runtime error and exit. */
static void runtimeError(CallFrame* callframe, const char* format, ...) {
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
// TODO(optimization): make this a macro and see if it speeds things up
void push(CallFrame* callFrame, Value value) {
    *callFrame->SP = value;
    callFrame->SP++;
}

// Pop (retrieve) value from stack
// TODO(optimization): make this a macro and see if it speeds things up
Value pop(CallFrame* callFrame) {
    callFrame->SP--;
    return *(callFrame->SP);
}

// Pop N values from the stack
// TODO(optimization): make this a macro and see if it speeds things up
void popN(CallFrame* callFrame, int n) {
    callFrame->SP -= n;
}

/**
 * peek(0) peeks the top of the stack
 * peek(1) peeks the second highest element of the stack
 */
static Value peek(CallFrame* callFrame, int distance) {
    return callFrame->SP[-1 - distance];
}

// Convert a Constant in the constants pool from the compiled bytecode into a runtime Value
Value bytecodeConstantToValue(CallFrame* callFrame, size_t constantIndex) {
    Constant constant = callFrame->codeObject->constantPool.values[constantIndex];
    switch (constant.type) {
        case CONST_TYPE_DOUBLE:
            return (Value){.type = TYPE_DOUBLE, .as = {.doubleVal = constant.as.number}};
        case CONST_TYPE_STRING:
        case CONST_TYPE_IDENTIFIER:
            return (Value){.type = TYPE_STRING, .as = {.stringVal = constant.as.string}};
        case CONST_TYPE_LAMBDA:
            return (Value){.type = TYPE_LAMBDA, .as = {.lambdaVal = constant.as.lambda}};
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
        case TYPE_LAMBDA:
            printf("%p", value.as.lambdaVal);
            break;
    };
    printf("\n");
}

// Apply an operation to two doubles, push double to stack
#define BINARY_NUMBER_OP(operation)                                                     \
    do {                                                                                \
        if (!IS_DOUBLE(peek(frame, 0)) || !IS_DOUBLE(peek(frame, 1))) {                 \
            runtimeError(frame, "Operands of arithmetic expression must be numbers.");  \
        }                                                                               \
        Value operand2 = pop(frame);                                                    \
        Value operand1 = pop(frame);                                                    \
        push(frame, DOUBLE_VAL(operand1.as.doubleVal operation operand2.as.doubleVal)); \
    } while (false)

// Apply an operation to two doubles, push boolean to stack
#define BINARY_NUMBER_OP_TRUTHY(operation)                                                   \
    do {                                                                                     \
        if (!IS_DOUBLE(peek(frame, 0)) || !IS_DOUBLE(peek(frame, 1))) {                      \
            runtimeError(frame, "Operands must be numbers.");                                \
        }                                                                                    \
        Value operand2 = pop(frame);                                                         \
        Value operand1 = pop(frame);                                                         \
        push(frame, BOOL_VAL((operand1.as.doubleVal operation operand2.as.doubleVal) == 1)); \
    } while (false)

/* Null, false, and the number 0 are falsey. Everything else is truthy */
static bool isFalsey(Value value) {
    return IS_NULL(value) || (IS_BOOLEAN(value) && !(value.as.booleanVal)) || (IS_DOUBLE(value) && (value.as.doubleVal == 0));
}

// Apply an operation to two booleans (converting if needed), push boolean to stack
#define BINARY_TRUTHY_OP(operation)                                               \
    do {                                                                          \
        Value operand2 = pop(frame);                                              \
        Value operand1 = pop(frame);                                              \
        push(frame, BOOL_VAL(!isFalsey(operand1) operation !isFalsey(operand2))); \
    } while (false)

#if DEBUG_VM
CallFrame* previousFrame = NULL;
#endif

/**
 * Execute one instruction in the VM.
 */
void step(VM* vm) {
    // Fetch the instruction at the instruction pointer
    CallFrame* frame = vm->currFrame;
    Bytecode* instruction = frame->IP;

#if DEBUG_VM
    if (previousFrame != frame) {
        printf(KCYN "%-16p" RESET, frame->codeObject);
    } else {
        printf(KGRY "                " RESET);
    }
    previousFrame = frame;
    printf(KGRY "%-4ld " RESET, frame->IP - frame->codeObject->bytecodeArray.values);
    printStack(frame->SP, vm->stack);
#endif

    // TODO: re-order switch based on frequency.
    switch (instruction->type) {
        case OP_LOAD_CONSTANT:
            push(frame, bytecodeConstantToValue(frame, instruction->maybeOperand1));
            break;
        case OP_DEFINE_GLOBAL_VAR:
        case OP_DEFINE_GLOBAL_VAL: {
            Value value = pop(frame);
            size_t constantIndex = instruction->maybeOperand1;
            Constant constant = frame->codeObject->constantPool.values[constantIndex];
            hashTableInsert(&vm->globals, constant.as.string, value);
            break;
        }
        case OP_GET_GLOBAL_VAL:
        case OP_GET_GLOBAL_VAR: {
            size_t constantIndex = instruction->maybeOperand1;
            Constant constant = frame->codeObject->constantPool.values[constantIndex];
            Value value = hashTableGet(&vm->globals, constant.as.string)->value;
            push(frame, value);
            break;
        }
        case OP_SET_GLOBAL_VAR: {
            Value value = pop(frame);
            size_t constantIndex = instruction->maybeOperand1;
            Constant constant = frame->codeObject->constantPool.values[constantIndex];
            hashTableInsert(&vm->globals, constant.as.string, value);
            break;
        }
        case OP_DEFINE_LOCAL_VAL_FAST:
        case OP_DEFINE_LOCAL_VAR_FAST:
            break;  // no action necessary to set locals. The compiler handles everything. 🤯
        case OP_GET_LOCAL_VAL_FAST:
        case OP_GET_LOCAL_VAR_FAST: {
            size_t stackIndex = instruction->maybeOperand1;
            Value value = frame->stackStart[stackIndex];
            push(frame, value);
            break;
        }
        case OP_SET_LOCAL_VAR_FAST: {
            // Copy the value at the top of the stack onto the slot corresponding to the local var
            size_t stackIndex = instruction->maybeOperand1;
            frame->stackStart[stackIndex] = pop(frame);
            break;
        }
        case OP_POPN: {
            size_t n = instruction->maybeOperand1;
            popN(frame, n);
            break;
        }
        case OP_PRINT: {
            Value value = pop(frame);
            printValue(value);
            break;
        }
        case OP_TRUE: {
            push(frame, BOOL_VAL(true));
            break;
        }
        case OP_NULL: {
            push(frame, NULL_VAL());
            break;
        }
        case OP_FALSE: {
            push(frame, BOOL_VAL(false));
            break;
        }
        case OP_UNARY_NEGATE: {
            Value value = pop(frame);
            if (value.type == TYPE_DOUBLE) {
                push(frame, DOUBLE_VAL(-value.as.doubleVal));
            } else {
                runtimeError(frame, "Operand of unary negation must be a number.");
            }
            break;
        }
        case OP_UNARY_NOT: {
            Value value = pop(frame);
            if (value.type == TYPE_BOOLEAN) {
                push(frame, BOOL_VAL(!value.as.booleanVal));
            } else {
                runtimeError(frame, "Operand of unary not must be a boolean.");
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
            Value value = pop(frame);
            // If condition is falsey, jump over the "then" branch
            if (isFalsey(value)) {
                size_t IPAfterThenBranch = instruction->maybeOperand1;
                // We have to subtract 1 because the instruction we jump to will be skipped (in `frame->IP++;` below).
                frame->IP = &(frame->codeObject->bytecodeArray.values[IPAfterThenBranch - 1]);
            }
            break;
        }
        case OP_JUMP: {
            size_t IPToJumpTo = instruction->maybeOperand1;
            // We have to subtract 1 because the instruction we jump to will be skipped (in `frame->IP++;` below).
            frame->IP = &(frame->codeObject->bytecodeArray.values[IPToJumpTo - 1]);
            break;
        }
        case OP_SWAP: {
            // Example: Swap(2) will swap [X X X A B C] -> [X X X C B A]
            size_t targetPositionFromTopOfStack = (frame->SP - vm->stack) - 1 - instruction->maybeOperand1;

            Value tempValue = vm->stack[targetPositionFromTopOfStack];
            vm->stack[targetPositionFromTopOfStack] = *(frame->SP - 1);
            *(frame->SP - 1) = tempValue;
            break;
        }
        case OP_LAMBDA: {
            size_t constantIndex = instruction->maybeOperand1;
            Constant constant = frame->codeObject->constantPool.values[constantIndex];
            if (constant.type != CONST_TYPE_LAMBDA) {
                runtimeError(frame, "Expected lambda in constant pool.");
            }
            Function* function = constant.as.lambda;
            push(frame, LAMBDA_VAL(function));
            break;
        }
        case OP_CALL: {
            Value callee = pop(frame);
            if (!IS_LAMBDA(callee)) {
                runtimeError(frame, "Can only call functions.");
            }
            Function* function = (Function*)callee.as.lambdaVal;

            if (vm->currFrame == &(vm->frames[FRAMES_MAX])) {
                runtimeError(frame, "Stack overflow. Too many frames.");
            }

            CallFrame* newFrame = frame + 1;
            newFrame->codeObject = function->code;
            newFrame->parameterCount = function->parameterCount;
            newFrame->IP = function->code->bytecodeArray.values;
            newFrame->SP = frame->SP;
            newFrame->stackStart = frame->SP - function->parameterCount;

            vm->currFrame = newFrame;
            break;
        }
        case OP_RETURN: {
            Value result = pop(frame);
            if (vm->currFrame == vm->frames) {
                runtimeError(frame, "Cannot return from main function");
            }

            // Put the return value on top of the stack
            (frame - 1)->SP -= frame->parameterCount;
            vm->currFrame = frame - 1;
            push(vm->currFrame, result);
            break;
        }
        default:
            // Handle any unknown or unimplemented opcodes.
            fprintf(stderr, "Unimplemented opcode %d\n", instruction->type);
            exit(EXIT_FAILURE);
    }

    // Move the instruction pointer to the next instruction
    frame->IP++;
}

/**
 * Execute code in the VM until it halts.
 */
void run(VM* vm) {
    // Find the last instruction so we know when to stop executing
    Bytecode* lastInstruction = &(vm->currFrame->codeObject->bytecodeArray.values[vm->currFrame->codeObject->bytecodeArray.used]);

#if DEBUG_VM
    printf("Started executing the VM.\n");
#endif

    // Loop if we're currently in a function or until the last instruction in the main function
    while (vm->currFrame != vm->frames || vm->currFrame->IP < lastInstruction) {
        step(vm);
    }

#if DEBUG_VM
    printf("Finished executing the VM.\n");
#endif
}