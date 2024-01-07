#include <stdio.h>
#include <stdlib.h>

#include "array.h"
#include "bytecode.h"
#include "compiler.h"
#include "debug.h"
#include "file.h"
#include "parser.h"
#include "scanner.h"
#include "token.h"
#include "vm.h"

static void executeFile(const char* path) {
    // First, read the file.
    char* sourceCode = readFile(path);

    // Then, scan characters into Tokens.
    Scanner scanner;
    initScanner(&scanner, sourceCode);
    TokenArray tokens = scanTokens(&scanner);
    printTokenList(tokens);

    // Then, parse the tokens into an Abstract Syntax Tree.
    ASTParser treeParser;
    initASTParser(&treeParser, tokens);
    Source* source = parseAST(&treeParser);
    printAST(source);

    // Then, compile the AST into bytecode.
    Compiler compiler;
    initCompiler(&compiler);
    BytecodeArray bytecode = compileAST(&compiler, source);
    printBytecodeArray(bytecode);

    // Then, execute the bytecode.
    VM vm;
    initVM(&vm, &bytecode);
    run(&vm);
}

int main(int argc, const char* argv[]) {
    if (argc >= 2) {
        executeFile(argv[1]);
    } else {
        fprintf(stderr, "Usage: delta [path]\n");
        exit(1);
    }

    return 0;
}