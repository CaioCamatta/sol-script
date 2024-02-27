#ifndef sol_script_compiler_h
#define sol_script_compiler_h

#include "bytecode.h"
#include "syntax.h"
#include "token.h"

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
    u_int8_t currentStackHeight;
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