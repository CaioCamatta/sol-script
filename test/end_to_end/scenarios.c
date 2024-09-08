#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../minunit.h"
#include "utils.h"

#define MAX_OUTPUT_SIZE 1024

char capturedOutput[MAX_OUTPUT_SIZE];

// Function to capture stdout
static int captureStdout(const char* input, char* output, size_t outputSize) {
    FILE* temp_stdout = tmpfile();
    if (!temp_stdout) return -1;

    FILE* original_stdout = stdout;
    stdout = temp_stdout;

    execute_solscript(input);

    fflush(stdout);
    stdout = original_stdout;

    rewind(temp_stdout);
    size_t bytes_read = fread(output, 1, outputSize - 1, temp_stdout);
    output[bytes_read] = '\0';

    fclose(temp_stdout);
    return 0;
}

#define SCENARIO(solCode) \
    const char* input = solCode;

#define EXPECT(expectedOutput)                                    \
    do {                                                          \
        captureStdout(input, capturedOutput, MAX_OUTPUT_SIZE);    \
        ASSERT(strcmp(capturedOutput, expectedOutput "\n") == 0); \
        return 0;                                                 \
    } while (0);

static int test_assignment_compound_assignment() {
    SCENARIO(
        "var a = 1;"
        "print a;"
        "a = a + 2;"
        "print a;"
        "var b = 5;"
        "b = b * 2;"
        "print b;");
    EXPECT("1.000000\n3.000000\n10.000000");
}

static int test_assignment_assign_to_uninitialized() {
    SCENARIO(
        "var a;"
        "a = 1;"
        "print a;");
    EXPECT("1.000000");
}

static int test_assignment_locals_same_name_different_blocks() {
    SCENARIO(
        "val a = { val c = 1; c + 2; };"
        "val b = { val c = 10; c + 2; };"
        "print a;"
        "print b;");
    EXPECT("3.000000\n12.000000");
}

static int test_assignment_var_assignment() {
    SCENARIO(
        "var a = 1;"
        "print a;"
        "a = 2;"
        "print a;");
    EXPECT("1.000000\n2.000000");
}

/*
static int test_invalid_assignment_number() {
    SCENARIO(
        "1 = 2;",
        ""); // Should produce an error
}

static int test_invalid_assignment_block() {
    SCENARIO(
        "{ val a = 1; a; } = 2;",
        ""); // Should produce an error
}

static int test_assign_to_undeclared() {
    SCENARIO(
        "a = 1;",
        ""); // Should produce an error
}

static int test_invalid_assignment_arithmetic() {
    SCENARIO(
        "(3 + 4) = 5;",
        ""); // Should produce an error
}

static int test_invalid_assignment_literal_parenthesized_number() {
    SCENARIO(
        "(1) = 2;",
        ""); // Should produce an error
}

static int test_val_reassignment_global() {
    SCENARIO(
        "val a = 2;"
        "a = 3;",
        ""); // Should produce an error
}

static int test_val_reassignment_local() {
    SCENARIO(
        "{"
        "    val a = 2;"
        "    a = 3;"
        "}",
        ""); // Should produce an error
}
*/

static int test_block_expressions_no_final_expression() {
    SCENARIO(
        "val a = { "
        "    val b = 2; "
        "};"
        "print a;");
    EXPECT("null");
}

static int test_block_expressions_no_semi_colon_after_block() {
    SCENARIO(
        "val a = { "
        "    val b = 2; "
        "    print \"In block 1\";"
        "    3;"
        "};"
        "print a;"
        "print 2 + { "
        "    print \"In block 2\"; "
        "    4;"
        "};");
    EXPECT("In block 1\n3.000000\nIn block 2\n6.000000");
}

static int test_block_expressions_nested_blocks_trivial() {
    SCENARIO(
        "val a = {{{ \"Correct!\"; };}};"
        "print a;");
    EXPECT("Correct!");
}

static int test_block_expressions_block_assignment() {
    SCENARIO(
        "val a = { val b = 1; b + 2; };"
        "if (a == 3) {"
        "    print \"Passed\";"
        "}");
    EXPECT("Passed");
}

static int test_block_expressions_simple_example() {
    SCENARIO(
        "val a = { "
        "    val b = 2; "
        "    3;"
        "};"
        "print a;");
    EXPECT("3.000000");
}

static int test_block_expressions_only_expression() {
    SCENARIO(
        "val a = { 3; };"
        "print a;");
    EXPECT("3.000000");
}

static int test_block_expressions_block_in_if_condition() {
    SCENARIO(
        "if ({ val a = 1; a > 0; }) {"
        "    print \"Passed\";"
        "}");
    EXPECT("Passed");
}

/*
static int test_block_expressions_empty_block() {
    SCENARIO(
        "2 + {};",
        ""); // Should produce an error
}
*/

static int test_call_expressions_call_in_binary_expression() {
    SCENARIO(
        "val double = lambda (n) {"
        "    n * 2;"
        "};"
        "val result = double(5) + 10;"
        "print result;");
    EXPECT("20.000000");
}

static int test_call_expressions_call_no_args() {
    SCENARIO(
        "val myFunc = lambda () {"
        "    print \"Called myFunc!\";"
        "};"
        "myFunc();");
    EXPECT("Called myFunc!");
}

static int test_call_expressions_call_with_block_expression_arg() {
    SCENARIO(
        "val applyOperation = lambda (a, operation) {"
        "    operation(a);"
        "};"
        "val result = applyOperation(5, lambda(x) { x * x; });"
        "print result;");
    EXPECT("25.000000");
}

static int test_call_expressions_nested_calls() {
    SCENARIO(
        "val addTwo = lambda (n) {"
        "    n + 2;"
        "};"
        "val multiplyByThree = lambda (n) {"
        "    n * 3;"
        "};"
        "val result = multiplyByThree(addTwo(5));"
        "print result;");
    EXPECT("21.000000");
}

static int test_call_expressions_call_with_expression_args() {
    SCENARIO(
        "val sum = lambda (a, b) {"
        "    a + b;"
        "};"
        "val x = 10;"
        "val y = 20;"
        "val result = sum(x + 5, y - 3);"
        "print result;");
    EXPECT("32.000000");
}

static int test_call_expressions_call_in_if_condition() {
    SCENARIO(
        "val isTen = lambda (n) {"
        "    n == 10;"
        "};"
        "val num = 10;"
        "if (isTen(num)) {"
        "    print \"10 is 10.\";"
        "} else {"
        "    print \"10 is not 10.\";"
        "}");
    EXPECT("10 is 10.");
}

static int test_functions_single_parameter() {
    SCENARIO(
        "val identity = lambda (x) { x; };"
        "print identity(5);"
        "print identity(\"hello\");");
    EXPECT("5.000000\nhello");
}

// static int test_functions_recursive() {
//     SCENARIO(
//         "val factorial = lambda (n) {"
//         "    if (n <= 1) {"
//         "        1;"
//         "    } else {"
//         "        n * factorial(n - 1);"
//         "    };"
//         "};"
//         "print factorial(5);");
//     EXPECT("120.000000");
// }

static int test_functions_modify_global() {
    SCENARIO(
        "var x = 10;"
        "val addToX = lambda (y) { x = x + y; };"
        "addToX(5);"
        "print x;");
    EXPECT("15.000000");
}

static int test_functions_multiple_parameters() {
    SCENARIO(
        "val add = lambda (a, b) { a + b; };"
        "print add(3, 4);");
    EXPECT("7.000000");
}

static int test_functions_return_complex_expression() {
    SCENARIO(
        "val complex = lambda(a, b, c, d) {"
        "    return a + b * (c - d);"
        "};"
        "print complex(1, 2, 3, 4);");
    EXPECT("-1.000000");
}

static int test_functions_return_in_nested_blocks() {
    SCENARIO(
        "val test = lambda() { "
        "    var x = 0;"
        "    while (true) { "
        "        if (x > 10) { "
        "            return x; "
        "        } "
        "        x = x + 1;"
        "    } "
        "    return 0; "
        "};"
        "print test();");
    EXPECT("11.000000");
}

static int test_functions_return_at_end_of_lambda() {
    SCENARIO(
        "val add = lambda(a, b) { "
        "    return a + b; "
        "};"
        "print add(3, 4);");
    EXPECT("7.000000");
}

static int test_functions_lambda_as_argument() {
    SCENARIO(
        "val applyFunc = lambda (f, x) { f(x); };"
        "val double = lambda (x) { x * 2; };"
        "print applyFunc(double, 5);");
    EXPECT("10.000000");
}

// static int test_functions_if_statement_in_lambda() {
//     SCENARIO(
//         "val calculate = lambda (x) { val result = x * 2; if (result > 10) { result + 1; } else { result - 1; } };"
//         "print calculate(6);");
//     EXPECT("13.000000");
// }

static int test_functions_return_in_if_statement() {
    SCENARIO(
        "val test = lambda(x) { "
        "    if (x > 0) { "
        "        return 1; "
        "    } else { "
        "        return -1; "
        "    } "
        "};"
        "print test(5);"
        "print test(-5);");
    EXPECT("1.000000\n-1.000000");
}

static int test_functions_no_parameters() {
    SCENARIO(
        "val getFortyTwo = lambda () { 42; };"
        "print getFortyTwo();");
    EXPECT("42.000000");
}

static int test_functions_lambda_returns_lambda() {
    SCENARIO(
        "val makeIdentiy = lambda () {"
        "    lambda (y) { y; };"
        "};"
        "val id = makeIdentiy();"
        "print id(10);");
    EXPECT("10.000000");
}

static int test_functions_return_in_nested_lambda() {
    SCENARIO(
        "val outer = lambda() {"
        "    val inner = lambda(x) {"
        "        return x * x;"
        "    };"
        "    return inner(4);"
        "};"
        "print outer();");
    EXPECT("16.000000");
}

static int test_functions_nested_calls() {
    SCENARIO(
        "val multiply = lambda (a, b) { a * b; };"
        "val add = lambda (a, b) { a + b; };"
        "print add(multiply(2, 3), multiply(4, 5));");
    EXPECT("26.000000");
}

/*
static int test_functions_error_return_global() {
    SCENARIO(
        "return;",
        ""); // Should produce an error: "Cannot return from global scope."
}

static int test_functions_immediately_invoked() {
    SCENARIO(
        "print \"SolScript doesn't support immediately-invoked function expressions yet\";"
        "print (lambda () { \"IIFE\"; })();",
        ""); // Should produce an error or unexpected behavior
}
*/

static int test_functions_chained_calls() {
    SCENARIO(
        "val makePrinter = lambda() { lambda(strToPrint) { print strToPrint; }; };"
        "makePrinter()(\"Success\");");
    EXPECT("Success")
}

static int test_if_statements_multiple_ifs() {
    SCENARIO(
        "if (false) { print true;} else { print false;}"
        "print 1;"
        "if (true) { print true;} else { print false;}"
        "print 2;"
        "if (false) { print true;}"
        "print 3;"
        "if (true) { print true;}"
        "print 4;"
        "if (false) { print true;} else { print false;}"
        "print 5;"
        "if (true) { print true;} else { print false;}"
        "print 6;"
        "if (false) { print true;}"
        "print 7;"
        "if (true) { print true;}"
        "print 8;");
    EXPECT("false\n1.000000\ntrue\n2.000000\n3.000000\ntrue\n4.000000\nfalse\n5.000000\ntrue\n6.000000\n7.000000\ntrue\n8.000000");
}

static int test_if_statements_nested_ifs() {
    SCENARIO(
        "if (true) { "
        "    print \"true-outer\";"
        "    if (true) { "
        "        print \"true-inner\";"
        "    } else { "
        "        print \"false-inner\";"
        "    }"
        "} else { "
        "    print \"false-outer\";"
        "}");
    EXPECT("true-outer\ntrue-inner");
}

static int test_if_statements_if_false_then_else() {
    SCENARIO(
        "if (false) { print 1;} else { print 2;}"
        "print 3;");
    EXPECT("2.000000\n3.000000");
}

static int test_if_statements_if_true_then_else() {
    SCENARIO(
        "if (true) { print 1;} else { print 2;}"
        "print 3;");
    EXPECT("1.000000\n3.000000");
}

static int test_if_statements_if_false_then() {
    SCENARIO(
        "if (false) { print 1;}"
        "print 3;");
    EXPECT("3.000000");
}

static int test_if_statements_complex_nested_ifs_expressions() {
    SCENARIO(
        "if (5 > 3) {"
        "    print \"5 is greater than 3\";"
        "    if (true && (10 >= 5)) {"
        "        print \"Both conditions are true\";"
        "        if ((5 + 5) == 10) {"
        "            print \"5 + 5 is indeed 10\";"
        "        } else {"
        "            print \"This shouldn't print\";"
        "        }"
        "    } else {"
        "        print \"This shouldn't print either\";"
        "    }"
        "    if (false || (7 <= 8)) {"
        "        print \"At least one condition is true\";"
        "        if ((3 * 3) != 9) {"
        "            print \"This shouldn't print\";"
        "        } else {"
        "            print \"3 * 3 is still 9\";"
        "            if (!(false)) {"
        "                print \"Negation of false is true\";"
        "            }"
        "        }"
        "    }"
        "} else {"
        "    print \"This shouldn't print\";"
        "}"
        "if (1 > 2) {"
        "    print \"This won't print because 1 is not greater than 2\";"
        "} else {"
        "    print \"As expected, 1 is not greater than 2\";"
        "    if (2 == 2) {"
        "        print \"2 is equal to 2, as expected\";"
        "    }"
        "}");
    EXPECT(
        "5 is greater than 3\n"
        "Both conditions are true\n"
        "5 + 5 is indeed 10\n"
        "At least one condition is true\n"
        "3 * 3 is still 9\n"
        "Negation of false is true\n"
        "As expected, 1 is not greater than 2\n"
        "2 is equal to 2, as expected");
}

static int test_if_statements_if_true_then() {
    SCENARIO(
        "if (true) { print 1;}"
        "print 3;");
    EXPECT("1.000000\n3.000000");
}

static int test_if_statements_no_brackets() {
    SCENARIO(
        "if (true) print \"Should print\";"
        "if (false) print \"Should NOT print\";"
        "if (true)"
        "    print \"Should print\";"
        "else"
        "    print \"Should NOT print\";"
        "if (false)"
        "    print \"Should NOT print\";"
        "else"
        "    print \"Should print\";"
        "if (false) {"
        "    print \"Should NOT print\";"
        "}"
        "else"
        "    print \"Should print\";"
        "if (false)"
        "    print \"Should NOT print\";"
        "else {"
        "    print \"Should print\";"
        "}"
        "val a = 20;"
        "if (a > 15) {"
        "    print \"Should print\";"
        "} else {"
        "    print \"Should NOT print\";"
        "}"
        "if (true) {"
        "    print \"Should print\";"
        "    print \"Should print\";"
        "}"
        "if (true)"
        "    print \"Should print\";"
        "    print \"Should print\";"
        "if (false)"
        "    print \"Should NOT print\";"
        "    print \"Should print\";");
    EXPECT(
        "Should print\n"
        "Should print\n"
        "Should print\n"
        "Should print\n"
        "Should print\n"
        "Should print\n"
        "Should print\n"
        "Should print\n"
        "Should print\n"
        "Should print\n"
        "Should print");
}

static int test_iteration_statements_while_loop_with_variable() {
    SCENARIO(
        "var i = 0;"
        "while (i < 3) {"
        "    print i;"
        "    i = i + 1;"
        "}");
    EXPECT("0.000000\n1.000000\n2.000000");
}

static int test_iteration_statements_while_loop_with_logical_operators() {
    SCENARIO(
        "var i = 0;"
        "var j = 0;"
        "while (i < 3 || j < 2) {"
        "    print i + j;"
        "    i = i + 1;"
        "    j = j + 1;"
        "}");
    EXPECT("0.000000\n2.000000\n4.000000");
}

static int test_iteration_statements_nested_while_loops() {
    SCENARIO(
        "var i = 0;"
        "while (i < 3) {"
        "    var j = 0;"
        "    while (j < 2) {"
        "        print i + j;"
        "        j = j + 1;"
        "    }"
        "    i = i + 1;"
        "}");
    EXPECT("0.000000\n1.000000\n1.000000\n2.000000\n2.000000\n3.000000");
}

static int test_iteration_statements_while_loop_with_block_expression() {
    SCENARIO(
        "var i = 0;"
        "while ({ i = i + 1; i <= 3; }) {"
        "    print i;"
        "}");
    EXPECT("1.000000\n2.000000\n3.000000");
}

/*
static int test_iteration_statements_simple_infinite_while_loop() {
    SCENARIO(
        "while (true) print \"Loop\";",
        ""); // This would run indefinitely, so it's not suitable for this test framework
}

static int test_iteration_statements_errors_missing_body() {
    SCENARIO(
        "while (true);",
        ""); // Should produce an error
}

static int test_iteration_statements_errors_missing_semicolon_after_body() {
    SCENARIO(
        "while (true) print Loop",
        ""); // Should produce an error
}

static int test_iteration_statements_errors_missing_condition() {
    SCENARIO(
        "while print Loop;",
        ""); // Should produce an error
}

static int test_iteration_statements_errors_missing_while_keyword() {
    SCENARIO(
        "(true) print Loop;",
        ""); // Should produce an error
}
*/

static int test_scope_consecutive_blocks() {
    SCENARIO(
        "{ "
        "    val a = 2; "
        "    print a;"
        "}"
        "{ "
        "    val a = 3; "
        "    print a;"
        "}"
        "{ "
        "    val a = 4; "
        "    print a;"
        "}");
    EXPECT("2.000000\n3.000000\n4.000000");
}

static int test_scope_nested_blocks() {
    SCENARIO(
        "{"
        "    val b = 5;"
        "    {"
        "        print b;"
        "    }"
        "}"
        "{"
        "    val b = 6;"
        "    {"
        "        {"
        "            print b;"
        "        }"
        "    }"
        "}");
    EXPECT("5.000000\n6.000000");
}

static int test_scope_global_vs_local() {
    SCENARIO(
        "val c = 7;"
        "{"
        "    {"
        "        {"
        "            print c;"
        "        }"
        "    }"
        "}");
    EXPECT("7.000000");
}

static int test_var_declaration_multiple_declarations() {
    SCENARIO(
        "var a = 1;"
        "print a;"
        "var b = \"hello\";"
        "print b;"
        "var c = true;"
        "print c;"
        "var d = { val x = 10; x; };"
        "print d;");
    EXPECT("1.000000\nhello\ntrue\n10.000000");
}

static int test_var_declaration_no_initializer() {
    SCENARIO(
        "var a;"
        "print a;");
    EXPECT("null");
}

static int test_var_declaration_simple_var_declaration() {
    SCENARIO(
        "var a = 5;"
        "print a;");
    EXPECT("5.000000");
}

static int test_var_declaration_global_declaration_local_assignment() {
    SCENARIO(
        "var a = 2;"
        "{ a = 3; }"
        "print a;");
    EXPECT("3.000000");
}

static int test_var_declaration_nested_var_declaration() {
    SCENARIO(
        "{"
        "  var a = 1;"
        "  {"
        "    val b = 2;"
        "    print a;"
        "    print b;"
        "  }"
        "  print a;"
        "}");
    EXPECT("1.000000\n2.000000\n1.000000");
}

/*
static int test_var_declaration_no_redeclaration() {
    SCENARIO(
        "var a = 1;"
        "var a = 2;",
        ""); // Should produce an error
}

static int test_var_declaration_use_before_declaration() {
    SCENARIO(
        "print a;"
        "var a = 1;",
        ""); // Should produce an error
}
*/

static int test_struct_simple_declaration() {
    SCENARIO(
        "var person = struct { name: \"Alice\"; age: 30; };"
        "print person.name;"
        "print person.age;");
    EXPECT("Alice\n30.000000");
}

static int test_struct_nested() {
    SCENARIO(
        "var address = struct { street: \"123 Main St\"; city: \"Anytown\"; };"
        "var person = struct { name: \"Bob\"; address: address; };"
        "print person.name;"
        "print person.address.street;"
        "print person.address.city;");
    EXPECT("Bob\n123 Main St\nAnytown");
}

static int test_struct_modification() {
    SCENARIO(
        "var car = struct { make: \"Toyota\"; model: \"Corolla\"; year: 2020; };"
        "print car.year;"
        "car.year = 2021;"
        "print car.year;");
    EXPECT("2020.000000\n2021.000000");
}

static int test_struct_in_function() {
    SCENARIO(
        "val createPerson = lambda (name, age) { struct { name: name; age: age; }; };"
        "var john = createPerson(\"John\", 25);"
        "print john.name;"
        "print john.age;");
    EXPECT("John\n25.000000");
}

static int test_struct_array_simulation() {
    SCENARIO(
        "var array = struct { "
        "   length: 3;"
        "   get: lambda(index) {"
        "       if (index == 0) { 10; }"
        "       else if (index == 1) { 20; }"
        "       else if (index == 2) { 30; }"
        "       else { null; }"
        "   };"
        "};"
        "print array.length;"
        "print array.get(0);"
        "print array.get(1);"
        "print array.get(2);"
        "print array.get(3);");
    EXPECT("3.000000\n10.000000\n20.000000\n30.000000\nnull");
}

static int test_struct_chained_access() {
    SCENARIO(
        "var complex = struct {"
        "   a: lambda() { struct { b: struct { c: lambda() { 42; }; }; }; };"
        "};"
        "print complex.a().b.c();");
    EXPECT("42.000000");
}