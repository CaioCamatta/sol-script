#include <stdio.h>
#include <stdlib.h>

#include "array.h"
#include "file.h"
#include "scanner.h"
#include "token.h"

static void executeFile(const char* path) {
    char* sourceCode = readFile(path);
    Scanner scanner;
    initScanner(&scanner, sourceCode);
    TokenArray tokens = scan(&scanner);

    for (size_t i = 0; i < tokens.used; i++) {
        printf("%s\n", tokenTypeStrings[tokens.values[i].type]);
    }
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