#ifndef sol_script_compiler_h
#define sol_script_compiler_h

#include <stdbool.h>

#include "bytecode.h"
#include "error.h"
#include "syntax.h"
#include "token.h"
#include "util/hash_table.h"

#define STACK_MAX 128

/**
 * Represents a local variable in the temporary stack maintained by the compiler.
 *
 * The original reason for this struct to exist is to allow SolScript to treat Local vs Global access
 * differently so we can optimize local variable access for speed.
 */
typedef struct {
    char* name;  // Null-terminated
    bool isModifiable;
} Local;

/**
 * Represents a global variable in the temporary hash table maintained by the compiler.
 *
 * The original reason for this struct to exist is to allow the compiler to prevent globals from being re-defined
 * and `val`s from being updated.
 */
typedef struct {
    char* name;
    bool isModifiable;
} Global;

/**
 * During Compiler execution, we use this struct to predict what the VM stack will look like.
 *
 * Note: stack prediction is necessary for so the compiler can tell the VM what stack slots to access.
 * For example, local variables are references via their position on the stack. So, to compile
 * a variable access we need to predict what the VM stack will look like during execution.
 */
typedef struct {
    u_int8_t currentStackHeight;  // The next empty spot on the stack
    Local tempStack[STACK_MAX];
} PredictedStack;

/**
 * Represents a saved state of the predicted stack at a point in time.
 * Used to restore stack state when exiting block scopes.
 */
typedef PredictedStack StackSnapshot;

typedef struct CompilerUnit CompilerUnit;
/**
 * Compiler struct for an individual compilation task, such as a function compilation.
 */
struct CompilerUnit {
    CompiledCodeObject compiledCodeObject;  // The code object being compiled
    PredictedStack predictedStack;          // A predictive copy of the VM's stack so we can know at compile time what position
                                            // local variables will be in. Holds only strings for variable names.
    bool isInGlobalScope;                   // Track whether the compiler is currently in the global scope instead of in a block.
                                            // This is used to distinguish between local variables and global variables.
    HashTable* globals;                     // Reference to the hash table that keep track of globals to prevent redefinition and enforce constant `val`s.
    CompilerUnit* enclosingCompilerUnit;    // Once this compiler unit is done compiling, return to the enclosing one
};

/**
 * Singleton compiler struct to facilitate compiling an AST into bytecode.
 */
typedef struct {
    Source* ASTSource;                 // Root of the AST
    HashTable globals;                 // A hash table to keep track of globals to prevent redefinition and enforce constant `val`s.
    CompilerUnit currentCompilerUnit;  // The current compiler unit being executed.
    ErrorArray errors;
} CompilerState;

/* Initialize a Compiler with an AST to be parsed */
void initCompilerState(CompilerState* compilerState, Source* ASTSource);

/* Free a Compiler state and all of its fields, except the ASTSource and the compiled code */
void freeCompilerStateButNotCompiledCode(CompilerState* compilerState);

/**
 * Compile an AST into bytecode.
 *
 * @param initializedRootCompilerState an initialized Compiler to use for compiling.
 */
CompiledCode compile(CompilerState* initializedRootCompilerState);

#endif