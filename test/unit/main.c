#include <stdio.h>

#include "minunit.h"
#include "src/compiler_test.c"
#include "src/parser_test.c"
#include "src/scanner_test.c"
#include "src/util/array_test.c"
#include "src/vm_test.c"

static void all_tests() {
    RUN_TEST(test_array);
    RUN_TEST(test_scanner);
    RUN_TEST(test_compiler);
    RUN_TEST(test_vm_addition);
    // RUN_TEST(test_parser_errorHandling);
    RUN_TEST(test_parser_simpleExpression);
}

int main(int argc, char **argv) {
    RUN_SUITE(all_tests);
}