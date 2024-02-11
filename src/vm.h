#ifndef sol_script_vm_h
#define sol_script_vm_h

#include "array.h"
#include "bytecode.h"
#include "token.h"
#include "util/hash_table.h"
#include "value.h"

#define STACK_MAX 256

/**
 * Scanner struct to facilitate scanning through a file.
 *
 * @param compiledCode compiled code including bytecode array and constants pool.
 * @param IP the instruction pointer.
 * @param stack the stack for our stack-based VM.
 * @param SP the stack pointer (we use an actual pointer instead of an int index for faster dereferencing)
 * */
typedef struct {
    CompiledCode compiledCode;
    Bytecode* IP;
    Value stack[STACK_MAX];
    HashTable globals;
    Value* SP;  // points to next slot to be used in the stack, e.g. [<val>, <val>, <empty> SP, <empty>, ...]
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
void initVM(VM* vm, CompiledCode compiledCode);

#endif