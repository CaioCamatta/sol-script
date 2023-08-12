#include "util/dynamic_array.h"

#include <assert.h>

#include "../../minunit.h"

char* test_dynamic_array() {
    mu_assert("error, getNum != 5", getNum() == 5);
    return 0;
}