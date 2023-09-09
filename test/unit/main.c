#include <stdio.h>

#include "minunit.h"
#include "src/scanner_test.c"
#include "src/util/array_test.c"

static void all_tests() {
    RUN_TEST(test_array);
    RUN_TEST(test_scanner);
}

int main(int argc, char **argv) {
    RUN_SUITE(all_tests);
}