#include "array.h"
#include "bytecode.h"
#include "compiler.h"
#include "debug.h"
#include "file.h"
#include "parser.h"
#include "scanner.h"
#include "token.h"
#include "vm.h"

void execute_solscript(const char* sourceCode) {
    // Scan characters into Tokens
    Scanner scanner;
    initScanner(&scanner, sourceCode);
    TokenArray tokens = scanTokens(&scanner);

    // Parse the tokens into an Abstract Syntax Tree
    ASTParser parser;
    initASTParser(&parser, tokens);
    Source* source = parseAST(&parser);

    // Compile the AST into bytecode
    CompilerState compiler;
    initCompilerState(&compiler, source);
    CompiledCode compiledCode = compile(&compiler);

    // Execute the compiledCode
    VM vm;
    initVM(&vm, compiledCode);
    run(&vm);

    // Clean up
    freeCompilerStateButNotCompiledCode(&compiler);
    freeParserButNotAST(&parser);
    freeSource(source);
    FREE_ARRAY(tokens);
}