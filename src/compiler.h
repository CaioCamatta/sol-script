#ifndef sol_script_compiler_h
#define sol_script_compiler_h

#include <stdbool.h>

#include "bytecode.h"
#include "syntax.h"
#include "token.h"

#define STACK_MAX 256

/**
 * Represents a local variable in the temporary stack maintained by the compiler.
 * This exists so we can optimize local variable access.
 */
typedef struct {
    char* name;  // Null-terminated
} Local;

/**
 * Compiler struct to facilitate compiling an AST into bytecode.
 *
 * @param currentStackHeight tracks the height of the VM stack at this point of compilation, starting at zero. (The compiler
 *                          can know in advance how tall the stack will be.)
 */
typedef struct {
    BytecodeArray compiledBytecode;
    ConstantPool constantPool;
    Source* ASTSource;  // Root of the AST

    bool isInGlobalScope;  // Track whether the compiler is currently in the global scope instead of in a block.
                           // This is used to distinguish between local variables and global variables.

    u_int8_t currentStackHeight;  // The next empty spot on the stack
    Local tempStack[STACK_MAX];   // A copy of the VM's stack so we can know at compile time what position
                                  // local variables will be in. Holds only strings for variable names.
} Compiler;

/* Initialize a Compiler with an AST to be parsed */
void initCompiler(Compiler* compiler, Source* ASTSource);

/**
 * Compile an AST into bytecode.
 *
 * @param compiler an initialized Compiler to use for compiling.
 */
CompiledCode compile(Compiler* compiler);

#endif