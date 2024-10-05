// Unit tests based on MinUnit (http://www.jera.com/techinfo/jtns/jtn002.html).
#ifndef minunit_h
#define minunit_h

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#include "util/colors.h"

#define SUCCESS_RETURN_CODE 0
#define FAILURE_RETURN_CODE 1

static int testsRun = 0;
static int testsFailed = 0;
static int assertionsRun = 0;
static int assertionsFailed = 0;

/**
 * Assert `test` expression is true. If not, print a standard error message.
 *
 * @param test an expression that evaluates to a truthy or falsy value.
 */
#define ASSERT(test)                             \
    do {                                         \
        assertionsRun++;                         \
        if (!(test)) {                           \
            assertionsFailed++;                  \
            printf(KRED                          \
                   "Failed: " #test "\n" RESET); \
            return FAILURE_RETURN_CODE;          \
        }                                        \
    } while (0)

/**
 * Assert two strings are equal. If not, print both.
 */
#define ASSERT_STRINGS_EQUAL(expectedStr, actualStr)       \
    do {                                                   \
        assertionsRun++;                                   \
        if (!(strcmp(expectedStr, actualStr "\n") == 0)) { \
            assertionsFailed++;                            \
            printf(KRED                                    \
                   "Failed: \"%s\""                        \
                   " not equal to " #actualStr "\n" RESET, \
                   expectedStr);                           \
            return FAILURE_RETURN_CODE;                    \
        }                                                  \
    } while (0)

/**
 * Assert `test` expression is true. If not, execute a print statement `printStatement`.
 *
 * @param test an expression that evaluates to a truthy or falsy value.
 * @param printStatement a print statement, e.g. `prinf("custom error message")`.
 */
#define ASSERT_WITH_MESSAGE(test, printStatement) \
    do {                                          \
        assertionsRun++;                          \
        if (!(test)) {                            \
            assertionsFailed++;                   \
            printStatement;                       \
            return FAILURE_RETURN_CODE;           \
        }                                         \
    } while (0)

/**
 * Execute a tests suite and keep track of successes and failures.
 *
 * @param test a function of return type `int`.
 */

// Buffer size for capturing test outputs. Adjust as necessary.
#define OUTPUT_BUFFER_SIZE 1024 * 1024

/**
 * Execute a tests suite and keep track of successes and failures.
 * Anything the test prints to std out will be buffered and only printed if
 * the test fails.
 *
 * (This macro can be significantly optimized; it doesn't even need to be a macro.)
 *
 * @param test a function of return type `int`.
 */
#define RUN_TEST(test)                                                \
    do {                                                              \
        fflush(stdout);                                               \
        int originalStdout = dup(STDOUT_FILENO);                      \
                                                                      \
        /* Create a temporary file for capturing output */            \
        char tempFileName[] = "/tmp/test_outputXXXXXX";               \
        int tempFileDescriptor = mkstemp(tempFileName);               \
        if (tempFileDescriptor == -1) {                               \
            perror("Could not create temp file for test output");     \
            exit(EXIT_FAILURE);                                       \
        }                                                             \
        FILE* tempFile = fdopen(tempFileDescriptor, "w+");            \
        if (!tempFile) {                                              \
            perror("Failed to open temp file for test output");       \
            close(tempFileDescriptor);                                \
            exit(EXIT_FAILURE);                                       \
        }                                                             \
                                                                      \
        /* Redirect stdout to the temporary file */                   \
        dup2(tempFileDescriptor, STDOUT_FILENO);                      \
                                                                      \
        /* Run the test */                                            \
        int testResult = test();                                      \
                                                                      \
        /* Flush and redirect stdout back to its original */          \
        fflush(stdout);                                               \
        dup2(originalStdout, STDOUT_FILENO);                          \
        close(originalStdout);                                        \
                                                                      \
        if (testResult) {                                             \
            /* Seek to the beginning of the temp file */              \
            rewind(tempFile);                                         \
                                                                      \
            printf(KBOLD KRED "\n%s\n" RESET KBOFF, #test);           \
            /* Read and print the contents of the temp file */        \
            char buffer[1024];                                        \
            while (fgets(buffer, sizeof(buffer), tempFile) != NULL) { \
                printf("%s", buffer);                                 \
            }                                                         \
            printf(KDGRY "------------------------------" RESET);     \
                                                                      \
            testsFailed++;                                            \
        }                                                             \
                                                                      \
        /* Clean up */                                                \
        fclose(tempFile);                                             \
        unlink(tempFileName);                                         \
                                                                      \
        testsRun++;                                                   \
    } while (0)

/**
 * @deprecated
 * Execute a tests suite and keep track of successes and failures.
 *
 * @param test a function of return type `int`.
 */
#define RUN_TEST_SIMPLE(test)        \
    do {                             \
        int testReturnCode = test(); \
        testsRun++;                  \
        if (testReturnCode)          \
            testsFailed++;           \
    } while (0)

/**
 * Execute test suits and print report.
 *
 * @param tests a void function containing any number of tests.
 */
#define RUN_SUITE(tests)                                                                  \
    do {                                                                                  \
        testsRun = 0;                                                                     \
        testsFailed = 0;                                                                  \
        assertionsRun = 0;                                                                \
        assertionsFailed = 0;                                                             \
        clock_t startTime = clock();                                                      \
        tests();                                                                          \
        clock_t endTime = clock();                                                        \
        double timeTaken = ((double)(endTime - startTime)) / CLOCKS_PER_SEC;              \
        printf("\n");                                                                     \
        printf("Assertions run: %d, failed: %d.\n", assertionsRun, assertionsFailed);     \
        printf("Tests run: %d, failed: %d. (%.5fs)\n", testsRun, testsFailed, timeTaken); \
        if (testsFailed) {                                                                \
            printf(KBOLD KRED "FAILED\n" RESET KBOFF);                                    \
        } else {                                                                          \
            printf(KGRN "PASSED\n" RESET);                                                \
        }                                                                                 \
        return testsFailed != 0;                                                          \
    } while (0)

#endif