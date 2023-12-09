#ifndef delta_compiler_h
#define delta_compiler_h

#include "bytecode.h"
#include "syntax.h"
#include "token.h"

/**
 * Compiler struct to facilitate compiling an AST into bytecode.
 *
 * */
typedef struct {
    BytecodeArray compiledBytecode;  // i.e., the compiled program
} Compiler;

/* Initialize a Compiler with an empty bytecode array */
void initCompiler(Compiler* compiler);

/**
 * Turn source code into an array of tokens.
 *
 * Throws exceptions in case of lexical errors.
 *
 * @param scanner an initialized Compiler to use for scanning.
 */
BytecodeArray compileAST(Compiler* compiler, Source ASTSource);

#endif