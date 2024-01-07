#ifndef delta_debug_h
#define delta_debug_h

#include "array.h"
#include "bytecode.h"
#include "parser.h"
#include "token.h"

void printToken(Token token);
void printTokenList(TokenArray tokenArray);
char const* tokenTypeStrings[];

void printAST(const Source* source);

void printBytecodeArray(BytecodeArray bytecodeArray);

#endif