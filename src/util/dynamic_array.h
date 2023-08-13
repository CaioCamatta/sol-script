#ifndef delta_dynamic_array_h
#define delta_dynamic_array_h

#include <stdio.h>
#include <stdlib.h>

#define INITIAL_ARRAY_SIZE (size_t)8

typedef struct {
    int *values;
    size_t used;
    size_t size;
} IntArray;

// Initialize array of any type with INITIAL_ARRAY_SIZE
#define INIT_ARRAY(arrayPtr, type)                                    \
    do {                                                              \
        arrayPtr->values = malloc(INITIAL_ARRAY_SIZE * sizeof(type)); \
        arrayPtr->used = 0;                                           \
        arrayPtr->size = INITIAL_ARRAY_SIZE;                          \
    } while (0)

// Insert element of any type into array
#define INSERT_ARRAY(arrayPtr, element, type)                                            \
    do {                                                                                 \
        if (arrayPtr->used == arrayPtr->size) {                                          \
            arrayPtr->size *= 2;                                                         \
            arrayPtr->values = realloc(arrayPtr->values, arrayPtr->size * sizeof(type)); \
        }                                                                                \
        arrayPtr->values[arrayPtr->used++] = element;                                    \
    } while (0)

// Free array of any type
#define FREE_ARRAY(arrayPtr)                 \
    do {                                     \
        free(arrayPtr->values);              \
        arrayPtr->values = NULL;             \
        arrayPtr->used = arrayPtr->size = 0; \
    } while (0)

#endif