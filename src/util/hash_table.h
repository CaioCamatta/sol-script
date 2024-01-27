#ifndef delta_hash_table_h
#define delta_hash_table_h

#include <stdbool.h>

#include "../value.h"

/**
 * A hash table entry.
 *
 * Semantically, if key = NULL and value = BOOL_VAL(true), we can consider the entry
 * deleted, aka a 'tombstone'.
 */
typedef struct {
    char* key;
    Value value;
} HashTableEntry;

#define IS_TOMBSTONE(entry) ((entry)->key == NULL && (entry)->value.type == TYPE_BOOLEAN && (entry)->value.as.booleanVal == true)
#define MAKE_TOMBSTONE(entry)            \
    do {                                 \
        (entry)->key = NULL;             \
        (entry)->value = BOOL_VAL(true); \
    } while (0)

/**
 * A hash table to store Values
 *
 * The methods herein use linear probing and FNA hashing.
 */
typedef struct {
    HashTableEntry* entries;
    int capacity;
    int size;  // Current number of elements in the hash table.
} HashTable;

// Initialize hash table with default capacity and empty entries (i.e., key=NULL and value=BOOL_VAL(false)).
void initHashTable(HashTable* table);

// Free the hash table.
void freeHashTable(HashTable* table);

// Insert an entry pair into the hash table. Returns true if successful.
bool hashTableInsert(HashTable* table, char* key, Value value);

// Search for a value by key in the hash table. Returns a pointer to the value or NULL if not found.
HashTableEntry* hashTableGet(HashTable* table, const char* key);

// Delete an entry from the hash table by key. Returns true if the entry was found and deleted.
bool hashTableDelete(HashTable* table, const char* key);

// Resize the hash table to a new capacity.
void hashTableResize(HashTable* table, int newCapacity);

#endif