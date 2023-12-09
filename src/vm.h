#ifndef delta_vm_h
#define delta_vm_h

#include "array.h"
#include "bytecode.h"
#include "token.h"
#include "value.h"

#define STACK_MAX 256

/**
 * Scanner struct to facilitate scanning through a file.
 *
 * @param instructions bytecode array.
 * @param IP the instruction pointer.
 * @param stack the stack for our stack-based VM.
 * @param SP the stack pointer (we use an actual pointer instead of an int index for faster dereferencing)
 * */
typedef struct {
    BytecodeArray* instructions;
    Bytecode* IP;
    Value stack[STACK_MAX];
    Value* SP;  // points to next empty value in the stack, e.g. [<val>, <val>, <empty> SP, <empty>, ...]
} VM;

/**
 * Execute code in the VM until it halts.
 *
 * @param vm an initialized VM.
 */
void run(VM* vm);

/**
 * Execute one instruciton in the VM
 */
void step(VM* vm);

/* Initialize VM with some source code. */
void initVM(VM* vm, BytecodeArray* bytecode);

#endif