#ifndef sol_script_compiler_h
#define sol_script_compiler_h

#include <stdbool.h>

#include "bytecode.h"
#include "syntax.h"
#include "token.h"
#include "util/hash_table.h"

#define STACK_MAX 256

/**
 * Represents a local variable in the temporary stack maintained by the compiler.
 *
 * The original reason for this struct to exist is to allow SolScript to treat Local vs Global access
 * differently so we can optimize local variable access for speed.
 */
typedef struct {
    char* name;  // Null-terminated
    bool isConstant;
} Local;

/**
 * Represents a global variable in the temporary hash table maintained by the compiler.
 *
 * The original reason for this struct to exist is to allow the compiler to prevent globals from being re-defined
 * and `val`s from being updated.
 */
typedef struct {
    char* name;
    bool isConstant;
} Global;

/**
 * During Compiler execution, we use this struct to predict what the VM stack will look like.
 *
 * This is necessary for various reasons. For example, local variables are references via
 * their position on the stack. So, to compile a variable access we need to predict what the VM
 * stack will look like during execution.
 */
typedef struct {
    u_int8_t currentStackHeight;  // The next empty spot on the stack
    Local tempStack[STACK_MAX];
} PredictedStack;

/**
 * Compiler struct to facilitate compiling an AST into bytecode.
 */
typedef struct {
    BytecodeArray compiledBytecode;
    ConstantPool constantPool;
    Source* ASTSource;  // Root of the AST

    bool isInGlobalScope;  // Track whether the compiler is currently in the global scope instead of in a block.
                           // This is used to distinguish between local variables and global variables.

    PredictedStack* predictedStack;  // A predictive copy of the VM's stack so we can know at compile time what position
                                     // local variables will be in. Holds only strings for variable names.
    HashTable tempGlobals;           // A hash table to keep track of globals to prevent redefinition and enforce constant `val`s.
} Compiler;

/* Initialize a Compiler with an AST to be parsed */
void initRootCompiler(Compiler* compiler, Source* ASTSource);

/**
 * Compile an AST into bytecode.
 *
 * @param compiler an initialized Compiler to use for compiling.
 */
CompiledCode compile(Compiler* compiler);

#endif