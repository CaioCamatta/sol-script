#include "util/dynamic_array.h"

#include <assert.h>

#include "../../minunit.h"

char* test_dynamic_array() {
    IntArray arr;
    IntArray* arrPtr = &arr;

    // Initialization
    INIT_ARRAY(arrPtr, int);
    ASSERT(arrPtr->size == INITIAL_ARRAY_SIZE);
    ASSERT(arrPtr->used == 0);

    // Fill up array with initial size
    for (size_t i = 0; i < INITIAL_ARRAY_SIZE; i++) {
        ASSERT(arrPtr->used == i);
        INSERT_ARRAY(arrPtr, i, int);
        ASSERT(arrPtr->values[i] == i);
    }
    ASSERT(arrPtr->size == INITIAL_ARRAY_SIZE);

    // Force array to grow in size
    INSERT_ARRAY(arrPtr, INITIAL_ARRAY_SIZE, int);
    ASSERT(arrPtr->values[INITIAL_ARRAY_SIZE] == INITIAL_ARRAY_SIZE);
    ASSERT(arrPtr->size == INITIAL_ARRAY_SIZE * 2);

    // Free memory
    FREE_ARRAY(arrPtr);
    ASSERT(arrPtr->used == 0);

    return 0;
}