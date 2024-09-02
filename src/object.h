#ifndef sol_script_object_h
#define sol_script_object_h

#include "util/hash_table.h"
#include "value.h"

// All objects in SolScript are Values. This file contains object-specific
// logic and exists mostly to prevent circular dependencies.

struct Obj;

struct ObjStruct {
    HashTable fields;
};

ObjStruct* newStruct();
void freeStruct(ObjStruct* structure);

#endif