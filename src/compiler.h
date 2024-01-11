#ifndef delta_compiler_h
#define delta_compiler_h

#include "bytecode.h"
#include "syntax.h"
#include "token.h"

/**
 * Compiler struct to facilitate compiling an AST into bytecode.
 */
typedef struct {
    BytecodeArray compiledBytecode;  // i.e., the compiled program
    Source* ASTSource;               // Root of the AST
} Compiler;

/* Initialize a Compiler with an AST to be parsed */
void initCompiler(Compiler* compiler, Source* ASTSource);

/**
 * Compile an AST into bytecode.
 *
 * @param compiler an initialized Compiler to use for compiling.
 */
BytecodeArray compile(Compiler* compiler);

/**
 * Reset the compiler and compile an AST into bytecode.
 *
 * @param source initialized Source to compile.
 */
BytecodeArray compileSource(Compiler* compiler, Source* ASTSource);

#endif