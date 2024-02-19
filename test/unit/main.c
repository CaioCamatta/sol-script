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
    RUN_TEST(test_hashTable_insert_and_get);
    RUN_TEST(test_hashTable_delete);
    RUN_TEST(test_hashTable_resize);

    RUN_TEST(test_scanner);

    RUN_TEST(test_compiler);
    RUN_TEST(test_compiler_print);
    RUN_TEST(test_compiler_val_declaration);
    RUN_TEST(test_compiler_variable_declaration_and_printing);
    RUN_TEST(test_add_constant_to_pool_no_duplicates);
    RUN_TEST(test_compiler_binary_equal);
    RUN_TEST(test_compiler_binary_not_equal);
    RUN_TEST(test_compiler_comparison_operations);
    RUN_TEST(test_compiler_logical_and_or_operations);
    RUN_TEST(test_compiler_multiplicative_expressions);
    RUN_TEST(test_compiler_unary_expressions);
    RUN_TEST(test_compiler_boolean_literal);
    RUN_TEST(test_compiler_string_literal);

    // RUN_TEST(test_parser_errorHandling);
    RUN_TEST(test_parser_simple_expression);
    RUN_TEST(test_parser_print_statement);
    RUN_TEST(test_parser_logical_or_expression);
    RUN_TEST(test_parser_logical_and_expression);
    RUN_TEST(test_parser_equality_expression);
    RUN_TEST(test_parser_comparison_expression);
    RUN_TEST(test_parser_multiplicative_expression);
    RUN_TEST(test_parser_unary_expression);
    RUN_TEST(test_parser_boolean_literal);
    RUN_TEST(test_parser_complex_expression);
    RUN_TEST(test_parser_nested_parentheses_expression);
    RUN_TEST(test_parser_variable_declaration_and_reading);
    RUN_TEST(test_parser_string_literal);

    RUN_TEST(test_vm_addition);
    RUN_TEST(test_vm_print);
    RUN_TEST(test_vm_set_and_get_global);
    RUN_TEST(test_vm_binary_equal);
    RUN_TEST(test_vm_comparison_operations);
    RUN_TEST(test_vm_logical_operations);
    RUN_TEST(test_vm_boolean_truthiness);
    RUN_TEST(test_vm_unary_not);
    RUN_TEST(test_vm_unary_negation);
}

int main(int argc, char **argv) {
    RUN_SUITE(all_tests);
}