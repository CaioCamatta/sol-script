// Unit tests based on MinUnit (http://www.jera.com/techinfo/jtns/jtn002.html).
#ifndef minunit_h
#define minunit_h

#define SUCCESS_RETURN_CODE 0
#define FAILURE_RETURN_CODE 1

// Colors for UNIX terminal
#define KRED "\x1B[31m"
#define KGRN "\x1B[32m"
#define KYEL "\x1B[33m"
#define RESET "\x1B[0m"  // Normal colour. You have to reset the terminal after using a colour.

static int testsRun = 0;
static int testsFailed = 0;
static int assertionsRun = 0;
static int assertionsFailed = 0;

/**
 * Assert `test` expression is true. If not, print a standard error message.
 *
 * @param test an expression that evaluates to a truthy or falsy value.
 */
#define ASSERT(test)                              \
    do {                                          \
        assertionsRun++;                          \
        if (!(test)) {                            \
            assertionsFailed++;                   \
            printf("ERROR - failed " #test "\n"); \
            return FAILURE_RETURN_CODE;           \
        }                                         \
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
#define RUN_TEST(test)               \
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
#define RUN_SUITE(tests)                                                              \
    do {                                                                              \
        tests();                                                                      \
        printf("\n");                                                                 \
        printf("Assertions run: %d, failed: %d.\n", assertionsRun, assertionsFailed); \
        printf("Tests run: %d, failed: %d.\n", testsRun, testsFailed);                \
        if (testsFailed) {                                                            \
            printf(KRED "FAILED\n" RESET);                                            \
        } else {                                                                      \
            printf(KGRN "PASSED\n" RESET);                                            \
        }                                                                             \
        return testsFailed != 0;                                                      \
    } while (0)

#endif