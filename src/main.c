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

static void repl() {
    // Use the same VM throughout the REPL session.
    VM vm;
    BytecodeArray bytecodeArray;
    INIT_ARRAY(bytecodeArray, Bytecode);
    initVM(&vm, &bytecodeArray);

    char input[1024];
    while (1) {
        printf("> ");
        if (!fgets(input, sizeof(input), stdin)) {
            printf("Error reading input.");
            continue;
        }

        Scanner scanner;
        ASTParser treeParser;
        Compiler compiler;

        TokenArray tokens = scanTokensFromString(&scanner, input);
        printTokenList(tokens);

        Source* source = parseASTFromTokens(&treeParser, &tokens);
        printAST(source);

        BytecodeArray newBytecode = compileSource(&compiler, source);
        printBytecodeArray(newBytecode);

        interpret(&vm, &newBytecode);

        FREE_ARRAY(tokens);
        freeSource(source);
        FREE_ARRAY(newBytecode);
    }
}

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
    initCompiler(&compiler, source);
    BytecodeArray bytecode = compile(&compiler);
    printBytecodeArray(bytecode);

    // Then, execute the bytecode.
    VM vm;
    initVM(&vm, &bytecode);
    run(&vm);
}

int main(int argc, const char* argv[]) {
    if (argc == 1) {
        repl();
    } else if (argc == 2) {
        executeFile(argv[1]);
    } else {
        fprintf(stderr, "Usage: delta [path]\n");
        exit(1);
    }

    return 0;
}