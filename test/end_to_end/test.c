#include <stdio.h>

#include "../minunit.h"
#include "scenarios.c"

void all_end_to_end_tests() {
    /* Assignment */
    RUN_TEST(test_assignment_compound_assignment);
    RUN_TEST(test_assignment_assign_to_uninitialized);
    RUN_TEST(test_assignment_locals_same_name_different_blocks);
    RUN_TEST(test_assignment_var_assignment);

    /* Block expressions */
    RUN_TEST(test_block_expressions_no_final_expression);
    RUN_TEST(test_block_expressions_no_semi_colon_after_block);
    RUN_TEST(test_block_expressions_nested_blocks_trivial);
    RUN_TEST(test_block_expressions_block_assignment);
    RUN_TEST(test_block_expressions_simple_example);
    RUN_TEST(test_block_expressions_only_expression);
    RUN_TEST(test_block_expressions_block_in_if_condition);

    /* Call expressions */
    RUN_TEST(test_call_expressions_call_in_binary_expression);
    RUN_TEST(test_call_expressions_call_no_args);
    RUN_TEST(test_call_expressions_call_with_block_expression_arg);
    RUN_TEST(test_call_expressions_nested_calls);
    RUN_TEST(test_call_expressions_call_with_expression_args);
    RUN_TEST(test_call_expressions_call_in_if_condition);

    /* Lambdas / Functions */
    RUN_TEST(test_functions_single_parameter);
    RUN_TEST(test_functions_modify_global);
    RUN_TEST(test_functions_multiple_parameters);
    RUN_TEST(test_functions_return_complex_expression);
    RUN_TEST(test_functions_return_in_nested_blocks);
    RUN_TEST(test_functions_return_at_end_of_lambda);
    RUN_TEST(test_functions_lambda_as_argument);
    RUN_TEST(test_functions_return_in_if_statement);
    RUN_TEST(test_functions_no_parameters);
    RUN_TEST(test_functions_lambda_returns_lambda);
    RUN_TEST(test_functions_return_in_nested_lambda);
    RUN_TEST(test_functions_nested_calls);
    // RUN_TEST(test_functions_if_statement_in_lambda); -- Needs to be fixed
    RUN_TEST(test_functions_recursive);
    RUN_TEST(test_functions_recursive_implicit_return);
    RUN_TEST(test_functions_chained_calls);

    /* If statements */
    RUN_TEST(test_if_statements_multiple_ifs);
    RUN_TEST(test_if_statements_nested_ifs);
    RUN_TEST(test_if_statements_if_false_then_else);
    RUN_TEST(test_if_statements_if_true_then_else);
    RUN_TEST(test_if_statements_if_false_then);
    RUN_TEST(test_if_statements_complex_nested_ifs_expressions);
    RUN_TEST(test_if_statements_if_true_then);
    RUN_TEST(test_if_statements_no_brackets);

    /* Iteration statements */
    RUN_TEST(test_iteration_statements_while_loop_with_variable);
    RUN_TEST(test_iteration_statements_while_loop_with_logical_operators);
    RUN_TEST(test_iteration_statements_nested_while_loops);
    RUN_TEST(test_iteration_statements_while_loop_with_block_expression);

    /* Scope */
    RUN_TEST(test_scope_consecutive_blocks);
    RUN_TEST(test_scope_nested_blocks);
    RUN_TEST(test_scope_global_vs_local);
    RUN_TEST(test_scope_nested_declarations);

    /* Variable declaration */
    RUN_TEST(test_var_declaration_multiple_declarations);
    RUN_TEST(test_var_declaration_no_initializer);
    RUN_TEST(test_var_declaration_simple_var_declaration);
    RUN_TEST(test_var_declaration_global_declaration_local_assignment);
    RUN_TEST(test_var_declaration_nested_var_declaration);

    /* Structs */
    RUN_TEST(test_struct_simple_declaration);
    RUN_TEST(test_struct_nested);
    RUN_TEST(test_struct_modification);
    RUN_TEST(test_struct_in_function);
    RUN_TEST(test_struct_array_simulation);
    RUN_TEST(test_struct_chained_access);
}

int run_all_end_to_end_tests() {
    RUN_SUITE(all_end_to_end_tests);
    return 0;
}