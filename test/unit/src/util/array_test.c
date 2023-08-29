#include "array.h"

#include "../../minunit.h"

int test_array() {
    IntArray arr;

    // Initialization
    INIT_ARRAY(arr, int);
    ASSERT(arr.size == INITIAL_ARRAY_SIZE);
    ASSERT(arr.used == 0);

    // Fill up array with initial size
    for (size_t i = 0; i < INITIAL_ARRAY_SIZE; i++) {
        ASSERT(arr.used == i);
        INSERT_ARRAY(arr, i, int);
        ASSERT(arr.values[i] == i);
    }
    ASSERT(arr.size == INITIAL_ARRAY_SIZE);

    // Force array to grow in size
    INSERT_ARRAY(arr, INITIAL_ARRAY_SIZE, int);
    ASSERT(arr.values[INITIAL_ARRAY_SIZE] == INITIAL_ARRAY_SIZE);
    ASSERT(arr.size == INITIAL_ARRAY_SIZE * 2);

    // Free memory
    FREE_ARRAY(arr);
    ASSERT(arr.used == 0);

    return SUCCESS_RETURN_CODE;
}