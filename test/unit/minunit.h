#define ASSERT(test)                                \
    do {                                            \
        if (!(test)) return "error, failed " #test; \
    } while (0)

#define RUN_TEST(test)               \
    do {                             \
        char *message = test();      \
        tests_run++;                 \
        if (message) return message; \
    } while (0)

extern int tests_run;