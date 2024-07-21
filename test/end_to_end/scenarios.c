#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../minunit.h"
#include "utils.h"

#define MAX_OUTPUT_SIZE 1024

char captured_output[MAX_OUTPUT_SIZE];

// Function to capture stdout
static int capture_stdout(const char* input, char* output, size_t output_size) {
    FILE* temp_stdout = tmpfile();
    if (!temp_stdout) return -1;

    FILE* original_stdout = stdout;
    stdout = temp_stdout;

    execute_solscript(input);

    fflush(stdout);
    stdout = original_stdout;

    rewind(temp_stdout);
    size_t bytes_read = fread(output, 1, output_size - 1, temp_stdout);
    output[bytes_read] = '\0';

    fclose(temp_stdout);
    return 0;
}

static int test_multiply_and_add() {
    const char* input =
        "val multiply = lambda (a, b) { a * b; };\n"
        "val add = lambda (a, b) { a + b; };\n"
        "print add(multiply(2, 3), multiply(4, 5));\n";

    capture_stdout(input, captured_output, MAX_OUTPUT_SIZE);

    ASSERT(strcmp(captured_output, "26.000000\n") == 0);
    return 0;
}