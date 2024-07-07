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
    // Use the same Compiler throughout the REPL session so we can add to the same constant pool
    Compiler compiler;
    initRootCompiler(&compiler, NULL);

    // Use the same VM throughout the REPL session so we can maintain runtime values
    VM vm;
    BytecodeArray bytecodeArray;
    INIT_ARRAY(bytecodeArray, Bytecode);
    CompiledCode compiledCode = (CompiledCode){
        .bytecodeArray = bytecodeArray,
        .constantPool = compiler.constantPool};

    initVM(&vm, compiledCode);

    char input[1024];
    while (1) {
        printf("> ");
        if (!fgets(input, sizeof(input), stdin)) {
            printf("Error reading input.");
            continue;
        }

        Scanner scanner;
        ASTParser treeParser;

        TokenArray tokens = scanTokensFromString(&scanner, input);
        printTokenList(tokens);

        Source* source = parseASTFromTokens(&treeParser, &tokens);
        printAST(source);

        // Reset compiled bytecode and feed new AST, but maintain same constant pool
        INIT_ARRAY(compiler.compiledBytecode, Bytecode);
        compiler.ASTSource = source;
        CompiledCode newCode = compile(&compiler);
        printCompiledCode(newCode);

        // Add new bytecode to VM
        for (int i = 0; i < newCode.bytecodeArray.used; i++) {
            vm.compiledCode.bytecodeArray.values[vm.compiledCode.bytecodeArray.used] = newCode.bytecodeArray.values[i];
            vm.compiledCode.bytecodeArray.used++;
        }
        run(&vm);
        printf("\n");

        FREE_ARRAY(tokens);
        freeSource(source);
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
    initRootCompiler(&compiler, source);
    CompiledCode compiledCode = compile(&compiler);
    printCompiledCode(compiledCode);

    // Then, execute the compiledCode.
    VM vm;
    initVM(&vm, compiledCode);
    run(&vm);
    printf("\n");
}

int main(int argc, const char* argv[]) {
    if (argc == 1) {
        repl();
    } else if (argc == 2) {
        executeFile(argv[1]);
    } else {
        fprintf(stderr, "Usage: sol [path]\n");
        exit(1);
    }

    return 0;
}