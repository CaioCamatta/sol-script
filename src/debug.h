#ifndef sol_script_debug_h
#define sol_script_debug_h

#include "array.h"
#include "bytecode.h"
#include "parser.h"
#include "token.h"

void printToken(Token token);
void printTokenList(TokenArray tokenArray);
char const* tokenTypeToString(TokenType tokenType);

void printAST(const Source* source);

void printCompiledCode(CompiledCode compiledCode);
void printCompiledCodeObject(CompiledCodeObject compiledCodeObject);

void printStack(const Value* topOfStack, const Value* bottomOfStack);

#endif