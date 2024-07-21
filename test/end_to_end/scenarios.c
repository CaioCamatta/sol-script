#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../minunit.h"
#include "utils.h"

#define MAX_OUTPUT_SIZE 1024

char capturedOutput[MAX_OUTPUT_SIZE];

// Function to capture stdout
static int captureStdout(const char* input, char* output, size_t outputSize) {
    FILE* temp_stdout = tmpfile();
    if (!temp_stdout) return -1;

    FILE* original_stdout = stdout;
    stdout = temp_stdout;

    execute_solscript(input);

    fflush(stdout);
    stdout = original_stdout;

    rewind(temp_stdout);
    size_t bytes_read = fread(output, 1, outputSize - 1, temp_stdout);
    output[bytes_read] = '\0';

    fclose(temp_stdout);
    return 0;
}

#define SCENARIO(solCode, expectedOutput)                         \
    do {                                                          \
        const char* input = solCode;                              \
        captureStdout(input, capturedOutput, MAX_OUTPUT_SIZE);    \
        ASSERT(strcmp(capturedOutput, expectedOutput "\n") == 0); \
        return 0;                                                 \
    } while (0);

static int test_multiply_and_add() {
    SCENARIO(
        "val multiply = lambda (a, b) { a * b; };"
        "val add = lambda (a, b) { a + b; };"
        "print add(multiply(2, 3), multiply(4, 5));",
        "26.000000");
}