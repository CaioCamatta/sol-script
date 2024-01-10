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
    Scanner scanner;
    ASTParser treeParser;
    Compiler compiler;

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

        // Fully reset the scanner, parser, and compiler and parse the input.
        initScanner(&scanner, input);
        TokenArray tokens = scanTokens(&scanner);
        printTokenList(tokens);

        initASTParser(&treeParser, tokens);
        Source* source = parseAST(&treeParser);
        printAST(source);

        initCompiler(&compiler);
        BytecodeArray newBytecode = compileAST(&compiler, source);
        printBytecodeArray(newBytecode);

        // Don't reset the VM for each line of input, since we want to keep the stack.
        addBytecode(&vm, &newBytecode);
        run(&vm);
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
    initCompiler(&compiler);
    BytecodeArray bytecode = compileAST(&compiler, source);
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