#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "array.h"
#include "bytecode.h"
#include "compiler.h"
#include "config.h"
#include "debug.h"
#include "file.h"
#include "parser.h"
#include "scanner.h"
#include "token.h"
#include "vm.h"

static void repl() {
    // Initialize a shared constant pool
    ConstantPool sharedConstantPool;
    INIT_ARRAY(sharedConstantPool, Constant);

    // Initialize the compiler with the shared constant pool
    CompilerState compiler;
    initCompilerState(&compiler, NULL);
    compiler.currentCompilerUnit.compiledCodeObject.constantPool = sharedConstantPool;

    // Initialize VM with empty bytecode but shared constant pool
    VM vm;
    CompiledCode initialCompiledCode = {0};
    INIT_ARRAY(initialCompiledCode.topLevelCodeObject.bytecodeArray, Bytecode);
    initialCompiledCode.topLevelCodeObject.constantPool = sharedConstantPool;
    initVM(&vm, initialCompiledCode);

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
        Source* source = parseASTFromTokens(&treeParser, &tokens);

        // Compile the new input
        INIT_ARRAY(compiler.currentCompilerUnit.compiledCodeObject.bytecodeArray, Bytecode);
        compiler.ASTSource = source;
        CompiledCode newCode = compile(&compiler);

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

    // Clean up
    FREE_ARRAY(sharedConstantPool);
    freeVM(&vm);
    freeCompilerState(&compiler);
}

static void executeFile(const char* path) {
    // First, read the file.
    char* sourceCode = readFile(path);

    // Then, scan characters into Tokens.
    Scanner scanner;
    initScanner(&scanner, sourceCode);
    TokenArray tokens = scanTokens(&scanner);

    // Then, parse the tokens into an Abstract Syntax Tree.
    ASTParser treeParser;
    initASTParser(&treeParser, tokens);
    Source* source = parseAST(&treeParser);

    // Then, compile the AST into bytecode.
    CompilerState compiler;
    initCompilerState(&compiler, source);
    CompiledCode compiledCode = compile(&compiler);

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
        fprintf(stderr, "Usage: sol [-d] [path]\n");
        exit(1);
    }

    return 0;
}