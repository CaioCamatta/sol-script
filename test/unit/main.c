#include <stdio.h>

#include "minunit.h"
#include "src/compiler_test.c"
#include "src/parser_test.c"
#include "src/scanner_test.c"
#include "src/util/array_test.c"
#include "src/util/hash_table_test.c"
#include "src/vm_test.c"

static void all_tests() {
    RUN_TEST(test_array);

    RUN_TEST(test_hashTable_init);
    RUN_TEST(test_hashTable_insertAndGet);
    RUN_TEST(test_hashTable_delete);
    RUN_TEST(test_hashTable_resize);

    RUN_TEST(test_scanner);

    RUN_TEST(test_compiler);
    RUN_TEST(test_compiler_print);

    RUN_TEST(test_vm_addition);
    RUN_TEST(test_vm_print);

    // RUN_TEST(test_parser_errorHandling);
    RUN_TEST(test_parser_simpleExpression);
    RUN_TEST(test_parser_printStatement);
    RUN_TEST(test_parser_logicalOrExpression);
}

int main(int argc, char **argv) {
    RUN_SUITE(all_tests);
}