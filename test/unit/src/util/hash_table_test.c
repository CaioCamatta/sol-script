#include "hash_table.h"

#include "../../../minunit.h"

// Hash table initialization test
int test_hashTable_init() {
    HashTable table;
    initHashTable(&table);

    ASSERT(table.entries != NULL);
    ASSERT(table.capacity > 0);
    ASSERT(table.size == 0);

    freeHashTable(&table);
    return SUCCESS_RETURN_CODE;
}

// Hash table insert and get test
int test_hashTable_insert_and_get() {
    HashTable table;
    initHashTable(&table);

    char* key = "key";
    Value value = DOUBLE_VAL(42.0);

    bool inserted = hashTableInsert(&table, key, value);
    ASSERT(inserted == true);

    HashTableEntry* foundEntry = hashTableGet(&table, key);
    ASSERT(foundEntry != NULL);
    ASSERT(foundEntry->value.type == TYPE_DOUBLE);
    ASSERT(foundEntry->value.as.doubleVal == 42.0);

    freeHashTable(&table);
    return SUCCESS_RETURN_CODE;
}

// Hash table delete test
int test_hashTable_delete() {
    HashTable table;
    initHashTable(&table);

    char* key = "key";
    Value value = DOUBLE_VAL(42.0);
    hashTableInsert(&table, key, value);

    bool deleted = hashTableDelete(&table, key);
    ASSERT(deleted == true);

    HashTableEntry* foundEntry = hashTableGet(&table, key);
    ASSERT(IS_TOMBSTONE(foundEntry));

    freeHashTable(&table);
    return SUCCESS_RETURN_CODE;
}

// Hash table resize test
int test_hashTable_resize() {
    HashTable table;
    initHashTable(&table);
    int oldCapacity = table.capacity;

    for (int i = 0; i < 100; i++) {
        char key[10];
        sprintf(key, "key%d", i);
        Value value = DOUBLE_VAL(i);
        hashTableInsert(&table, key, value);
    }

    ASSERT(table.capacity > oldCapacity);
    ASSERT(table.size == 100);
    ASSERT((table.capacity & (table.capacity - 1)) == 0);  // capacity is multiple of 2
    ASSERT(table.size / table.capacity < 1);

    for (int i = 0; i < 100; i++) {
        char key[10];
        sprintf(key, "key%d", i);
        HashTableEntry* foundEntry = hashTableGet(&table, key);
        ASSERT(foundEntry != NULL);
        ASSERT(foundEntry->value.as.doubleVal == i);
    }

    freeHashTable(&table);
    return SUCCESS_RETURN_CODE;
}
