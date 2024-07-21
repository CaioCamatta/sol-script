#ifndef unit_test_utils_h
#define unit_test_utils_h

// This file contains utilities to use across unit tests

/**
 * Macro to capture the std output of a test. It can be used to confirm that a correct values
 * are being printed, for example.
 *
 * Uses a temporary file.
 */
#define BUFFER_SIZE 1024
#define CAPTURE_PRINT_OUTPUT(executionBlock, assertionsBlock)    \
    do {                                                         \
        char buffer[BUFFER_SIZE] = {0};                          \
        FILE* old_stdout = stdout;                               \
        FILE* temp_file = tmpfile();                             \
        if (!temp_file) {                                        \
            perror("Failed to open temporary file");             \
            exit(EXIT_FAILURE);                                  \
        }                                                        \
        stdout = temp_file;                                      \
                                                                 \
        executionBlock                                           \
                                                                 \
            fflush(stdout);                                      \
        stdout = old_stdout;                                     \
        fseek(temp_file, 0, SEEK_SET);                           \
        fread(buffer, sizeof(char), BUFFER_SIZE - 1, temp_file); \
        fclose(temp_file);                                       \
        assertionsBlock                                          \
    } while (0)

#endif