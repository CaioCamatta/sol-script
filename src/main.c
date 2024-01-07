#include <stdio.h>
#include <stdlib.h>

#include "array.h"
#include "file.h"
#include "parser.h"
#include "scanner.h"
#include "token.h"

static void executeFile(const char* path) {
    // First, read the file.
    char* sourceCode = readFile(path);

    // Then, scan characters into Tokens.
    Scanner scanner;
    initScanner(&scanner, sourceCode);
    TokenArray tokens = scan(&scanner);
    printTokenList(tokens);

    // Then, parse the tokens into an Abstract Syntax Tree.
    // ASTParser treeParser;
    // initASTParser(&treeParser, tokens);
    // Source* source = parseAST(&treeParser);
    // printAST(source);
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