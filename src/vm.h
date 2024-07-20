#ifndef sol_script_vm_h
#define sol_script_vm_h

#include "array.h"
#include "bytecode.h"
#include "token.h"
#include "util/hash_table.h"
#include "value.h"

#define STACK_MAX 128
#define FRAMES_MAX 8  // Max number of nested function calls.

typedef struct {
    CompiledCodeObject* codeObject;
    u_int8_t parameterCount;
    Bytecode* IP;       // Instruction pointer
    Value* SP;          // Stack pointer
    Value* stackStart;  // First value belonging to this call frame's stack
} CallFrame;

/**
 * The SolScript virtual machine. Executes call frames.
 *
 * Holds the global variables and a stack that is shared across call frames.
 *
 * @param stack the stack for our stack-based VM.
 * @param globals hash table containing global variables
 * @param frames the call frames being executed
 * @param currFrame the current call frame being executed.
 * */
typedef struct {
    Value stack[STACK_MAX * FRAMES_MAX];
    HashTable globals;
    CallFrame frames[FRAMES_MAX];
    CallFrame* currFrame;
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

/* Free a VM and its structs. */
void freeVM(VM* vm);

#endif