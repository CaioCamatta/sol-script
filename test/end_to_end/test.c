#include <stdio.h>

#include "../minunit.h"
#include "scenarios.c"

void all_end_to_end_tests() {
    RUN_TEST(test_multiply_and_add);
}

int run_all_end_to_end_tests() {
    RUN_SUITE(all_end_to_end_tests);
    return 0;
}