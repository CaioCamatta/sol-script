#ifndef sol_script_array_h
#define sol_script_array_h

#include <stdio.h>
#include <stdlib.h>

#include "syntax.h"
#include "token.h"

#define INITIAL_ARRAY_SIZE (size_t)8

typedef struct {
    int *values;
    size_t used;
    size_t size;
} IntArray;

typedef struct {
    Token *values;
    size_t used;
    size_t size;
} TokenArray;

// Initialize array of any type with INITIAL_ARRAY_SIZE
#define INIT_ARRAY(array, type)                                   \
    do {                                                          \
        array.values = malloc(INITIAL_ARRAY_SIZE * sizeof(type)); \
        array.used = 0;                                           \
        array.size = INITIAL_ARRAY_SIZE;                          \
    } while (0)

// Insert element of any type into array
#define INSERT_ARRAY(array, element, type)                                   \
    do {                                                                     \
        if (array.used == array.size) {                                      \
            array.size *= 2;                                                 \
            array.values = realloc(array.values, array.size * sizeof(type)); \
        }                                                                    \
        array.values[array.used++] = element;                                \
    } while (0)

// Free array of any type
#define FREE_ARRAY(array)            \
    do {                             \
        free(array.values);          \
        array.values = NULL;         \
        array.used = array.size = 0; \
    } while (0)

#endif