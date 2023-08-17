#include <stdio.h>
#include <stdlib.h>

#include "file.h"
#include "scanner.h"

static void executeFile(const char* path) {
    char* sourceCode = readFile(path);
    printf(sourceCode);
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