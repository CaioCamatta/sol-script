#include <stdio.h>

#include "colors.h"
#include "end_to_end/test.c"
#include "minunit.h"
#include "unit/test.c"

int main(int argc, char **argv) {
    printf(KBOLD "Executing unit tests." KBOFF);
    run_all_unit_tests();
    printf(KBOLD "\nExecuting end-to-end tests.\n" KBOFF);
    run_all_end_to_end_tests();
    return 0;
}