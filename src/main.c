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
    CompilerState compiler;
    initCompilerState(&compiler, NULL);

    // Use the same VM throughout the REPL session so we can maintain runtime values
    VM vm;
    BytecodeArray bytecodeArray;
    INIT_ARRAY(bytecodeArray, Bytecode);
    CompiledCode compiledCode = (CompiledCode){
        .topLevelCodeObject = compiler.currentCompilerUnit.compiledCodeObject};

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
        INIT_ARRAY(compiler.currentCompilerUnit.compiledCodeObject.bytecodeArray, Bytecode);
        compiler.ASTSource = source;
        CompiledCode newCode = compile(&compiler);
        printCompiledCode(newCode);

        // Add new bytecode to VM
        for (int i = 0; i < newCode.topLevelCodeObject.bytecodeArray.used; i++) {
            vm.frames[0].codeObject->bytecodeArray.values[vm.frames[0].codeObject->bytecodeArray.used] = newCode.topLevelCodeObject.bytecodeArray.values[i];
            vm.frames[0].codeObject->bytecodeArray.used++;
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
    CompilerState compiler;
    initCompilerState(&compiler, source);
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