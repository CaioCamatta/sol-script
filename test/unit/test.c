#include <stdio.h>

#include "../minunit.h"
#include "src/compiler_test.c"
#include "src/parser_test.c"
#include "src/scanner_test.c"
#include "src/util/array_test.c"
#include "src/util/hash_table_test.c"
#include "src/vm_test.c"

void all_unit_tests() {
    // Array (util) tests
    RUN_TEST(test_array);

    // Hash table (util) tests
    RUN_TEST(test_hashTable_init);
    RUN_TEST(test_hashTable_insert_and_get);
    RUN_TEST(test_hashTable_delete);
    RUN_TEST(test_hashTable_resize);

    // Scanner tests
    RUN_TEST(test_scanner);

    // Parser tests
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
    RUN_TEST(test_parser_block_statement);
    RUN_TEST(test_parser_if_statement_true_branch_only);
    RUN_TEST(test_parser_if_statement_with_else_branch);
    RUN_TEST(test_parser_block_expression_simple);
    RUN_TEST(test_parser_block_expression_nested);
    RUN_TEST(test_parser_block_expression_with_statements);
    RUN_TEST(test_parser_block_expression_as_if_condition);
    RUN_TEST(test_parser_var_declaration);
    RUN_TEST(test_parser_var_declaration_with_initializer);
    RUN_TEST(test_parser_assignment);
    RUN_TEST(test_parser_iteration_statement_no_curlys);
    RUN_TEST(test_parser_iteration_statement_no_parentheses_no_curlys);
    RUN_TEST(test_parser_iteration_statement_with_block);
    RUN_TEST(test_parser_lambda_expression_no_parameters);
    RUN_TEST(test_parser_lambda_expression_single_parameter);
    RUN_TEST(test_parser_lambda_expression_multiple_parameters);
    RUN_TEST(test_parser_lambda_expression_no_last_expression_in_block);
    RUN_TEST(test_parser_call_with_args);
    RUN_TEST(test_parser_call_in_binary_expression);
    RUN_TEST(test_parser_call_no_args);
    RUN_TEST(test_parser_recursive_call);
    RUN_TEST(test_parser_call_with_block_expression_arg);
    RUN_TEST(test_parser_nested_calls);
    RUN_TEST(test_parser_call_with_expression_args);
    RUN_TEST(test_parser_call_in_if_condition);
    RUN_TEST(test_parser_chained_calls);
    RUN_TEST(test_parser_simple_return);
    RUN_TEST(test_parser_return_without_expression);
    RUN_TEST(test_parser_return_in_lambda);
    RUN_TEST(test_parser_multiple_returns);
    RUN_TEST(test_parser_member_expression_simple);
    RUN_TEST(test_parser_member_and_call_expression);
    RUN_TEST(test_parser_complex_member_and_call_expression);
    RUN_TEST(test_parser_nested_call_expression);
    RUN_TEST(test_parser_empty_struct);
    RUN_TEST(test_parser_struct_with_single_declaration);
    RUN_TEST(test_parser_struct_with_multiple_declarations);
    RUN_TEST(test_parser_struct_with_nested_struct);

    // Compiler tests
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
    RUN_TEST(test_compiler_stack_height_expression_and_val);
    RUN_TEST(test_compiler_single_block_statement_with_locals);
    RUN_TEST(test_compiler_nested_blocks_with_global_and_local_vars);
    RUN_TEST(test_compiler_if_statement_no_else);
    RUN_TEST(test_compiler_if_statement_with_else);
    RUN_TEST(test_compiler_nested_if_statements);
    RUN_TEST(test_compiler_block_expression_simple);
    RUN_TEST(test_compiler_block_expression_nested);
    RUN_TEST(test_compiler_block_expression_with_statements);
    RUN_TEST(test_compiler_var_declaration_and_assignment_global);
    RUN_TEST(test_compiler_var_declaration_and_assignment_local);
    RUN_TEST(test_compiler_global_declaration_and_local_assignment);
    RUN_TEST(test_compiler_iteration_statement_simple);
    RUN_TEST(test_compiler_iteration_statement_with_variable);
    RUN_TEST(test_compiler_lambda_expression_simple);
    RUN_TEST(test_compiler_lambda_expression_nested);
    RUN_TEST(test_compiler_call_expression_simple);
    RUN_TEST(test_compiler_call_expression_nested);
    RUN_TEST(test_compiler_chained_calls);
    RUN_TEST(test_compiler_lambda_with_return);
    RUN_TEST(test_compiler_lambda_with_conditional_returns);
    RUN_TEST(test_compiler_simple_struct);
    RUN_TEST(test_compiler_struct_field_access);
    RUN_TEST(test_compiler_nested_struct);
    RUN_TEST(test_compiler_struct_assignment);

    // VM tests
    RUN_TEST(test_vm_addition);
    RUN_TEST(test_vm_print);
    RUN_TEST(test_vm_set_and_get_global);
    RUN_TEST(test_vm_binary_equal);
    RUN_TEST(test_vm_comparison_operations);
    RUN_TEST(test_vm_logical_operations);
    RUN_TEST(test_vm_boolean_truthiness);
    RUN_TEST(test_vm_unary_not);
    RUN_TEST(test_vm_unary_negation);
    RUN_TEST(test_vm_print_string_literal);
    RUN_TEST(test_vm_simple_block_statement_and_cleanup);
    RUN_TEST(test_vm_nested_blocks_with_global_and_local_vars);
    RUN_TEST(test_vm_nested_if_statements);
    RUN_TEST(test_vm_simple_block_expression);
    RUN_TEST(test_vm_block_expression_with_statements);
    RUN_TEST(test_vm_block_expression_as_if_condition);
    RUN_TEST(test_vm_var_declaration_and_assignment_global);
    RUN_TEST(test_vm_var_declaration_and_assignment_local);
    RUN_TEST(test_vm_block_expression_with_val_declarations);
    RUN_TEST(test_vm_var_val_declarations_in_nested_blocks);
    RUN_TEST(test_vm_var_assignment_global_and_local);
    RUN_TEST(test_vm_global_declaration_and_local_assignment);
    RUN_TEST(test_vm_iteration_statement_simple);
    RUN_TEST(test_vm_iteration_statement_with_variable);
    RUN_TEST(test_vm_nested_iteration_statements);
    RUN_TEST(test_vm_lambda_no_params);
    RUN_TEST(test_vm_lambda_one_param);
    RUN_TEST(test_vm_lambda_two_params);
    RUN_TEST(test_vm_lambda_nested_calls);
    RUN_TEST(test_vm_simple_struct);
    RUN_TEST(test_vm_nested_structs);
    RUN_TEST(test_vm_struct_field_assignment);
    RUN_TEST(test_vm_struct_in_function_call);
}

int run_all_unit_tests() {
    RUN_SUITE(all_unit_tests);
}