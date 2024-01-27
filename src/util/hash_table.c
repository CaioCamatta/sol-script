#include "hash_table.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

/**
 * I chose a load factor of 0.67 because Python uses 2/3 as its load factor so it seems
 * like a good start. https://hg.python.org/cpython/file/52f68c95e025/Include/dictobject.h#l64
 * TODO: Experiment with the load factor to see what works best.
 */
#define MAX_LOAD_FACTOR 0.67

/**
 * Hash a null-terminated string using the FNV-1a algorithm.
 * I selected FNV due to its performance, simplicity, and low collision rate.
 * TODO: Test MurmurOAAT and see how it compares. https://stackoverflow.com/a/69812981.
 */
static uint32_t hash(const char* key) {
    uint32_t h = 2166136261UL;
    const uint8_t* data = (const uint8_t*)key;
    for (int i = 0; data[i] != '\0'; i++) {
        h ^= data[i];
        h *= 16777619;
    }
    return h;
}

// Initialize a hash table with a specific capacity.
static void initHashTableWithCapacity(HashTable* table, int capacity) {
    // Assert capacity is a power of two so we can use bitwise AND instead of modulo in hashTableGet().
    assert(capacity > 0 && (capacity & (capacity - 1)) == 0);

    table->capacity = capacity;
    table->size = 0;
    table->entries = malloc(table->capacity * sizeof(HashTableEntry));

    for (int i = 0; i < table->capacity; i++) {
        table->entries[i].key = NULL;
        table->entries[i].value = BOOL_VAL(false);  // Empty value. NOT a tombstone.
    }
}

void initHashTable(HashTable* table) {
    // Start with capacity 8; many programming languages do the same (e.g. Python)
    initHashTableWithCapacity(table, 8);
}

void freeHashTable(HashTable* table) {
    for (int i = 0; i < table->capacity; i++) {
        if (table->entries[i].key != NULL) {
            free(table->entries[i].key);
        }
    }
    free(table->entries);
}

HashTableEntry* hashTableGet(HashTable* table, const char* key) {
    // Math tells us that n % 2^i = n & (2^i - 1) if n is positive
    // Bitwise and is much faster than modulo.
    // The table capacity is always a multiple of two, enforced in initHashTableWithCapacity().
    int index = hash(key) & (table->capacity - 1);

    // Keep track of the first tombstone so we can re-use it.
    HashTableEntry* firstTombstone = NULL;

    for (int i = 0; i < table->capacity; i++) {
        int probeIndex = (index + i) % table->capacity;
        HashTableEntry* entry = &table->entries[probeIndex];

        if (entry->key == NULL) {
            if (IS_TOMBSTONE(entry) && firstTombstone == NULL) {
                // If it's the first tombstone encountered, save it.
                firstTombstone = entry;
            } else {
                // If it's a non-tombstone spot, the key isn't in the hashtable.
                // So, return the first tombstone encountered if any, else NULL
                return firstTombstone ? firstTombstone : NULL;
            }
        } else if (strcmp(entry->key, key) == 0) {  // TODO: internallize strings so we can use
                                                    // pointer comparison instead of strcmp
            //  Found the key
            return entry;
        }
    }

    // If the entire table was full of tombstones, return the first tombstone encountered
    return firstTombstone;
}

bool hashTableInsert(HashTable* table, char* key, Value value) {
    // Double the size of the table if load is too high
    if ((double)table->size / table->capacity > MAX_LOAD_FACTOR) {
        hashTableResize(table, table->capacity * 2);
    }

    // Find an entry to insert into
    HashTableEntry* entry = hashTableGet(table, key);

    // Only add count if we are not replacing a tombstone
    if (!IS_TOMBSTONE(entry)) table->size++;

    entry->key = key;
    entry->value = value;

    return true;
}

bool hashTableDelete(HashTable* table, const char* key) {
    // Find the entry.
    HashTableEntry* entry = hashTableGet(table, key);
    if (entry->key == NULL) return false;

    // Place a tombstone in the entry.
    MAKE_TOMBSTONE(entry);

    // We purposely don't reduce the size of the table so that tombstones count towards the load factor.
    // This has pros and cons. I'm leaving it like this for now because it's simpler and prevents long
    // loops when searching for an entry if there are many tombstones.
    // TODO: experiment with not counting tombstones towards the load factor.

    return true;
}

void hashTableResize(HashTable* table, int newCapacity) {
    HashTable newTable;
    initHashTableWithCapacity(&newTable, newCapacity);

    for (int i = 0; i < table->capacity; i++) {
        HashTableEntry* entry = &table->entries[i];
        if (entry->key == NULL) continue;  // Skip empty spaces and tombstones

        hashTableInsert(&newTable, entry->key, entry->value);
    }

    freeHashTable(table);
    *table = newTable;
}