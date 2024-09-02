#include "object.h"

#include <stdlib.h>

#include "value.h"

ObjStruct* newStruct() {
    ObjStruct* structure = (ObjStruct*)malloc(sizeof(ObjStruct));
    initHashTable(&structure->fields);
    return structure;
}

void freeStruct(ObjStruct* structure) {
    freeHashTable(&structure->fields);
    free(structure);
}