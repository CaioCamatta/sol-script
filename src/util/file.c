#include "util/file.h"

#include <stdio.h>
#include <stdlib.h>

char *readFile(const char *path) {
    FILE *file = fopen(path, "rb");

    if (file == NULL) {
        fprintf(stderr, "Error opening file file \"%s\".", path);
        exit(1);
    }

    // Figure out how large the file is
    fseek(file, 0, SEEK_END);
    size_t fileSizeBytes = ftell(file);
    fseek(file, 0, SEEK_SET);

    // Allocate memory for string
    char *stringFile = (char *)malloc(fileSizeBytes + 1);
    if (stringFile == NULL) {
        fprintf(stderr, "Not enough memory to allocate file \"%s\" as a string.", path);
        fclose(file);
        exit(1);
    }

    // Read
    size_t bytesRead = fread(stringFile, sizeof(char), fileSizeBytes, file);
    if (bytesRead < fileSizeBytes) {
        fprintf(stderr, "Could not read file \"%s\".", path);
        free(stringFile);
        fclose(file);
        exit(1);
    }

    stringFile[bytesRead] = '\0';
    fclose(file);

    return stringFile;
}