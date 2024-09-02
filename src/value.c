#include "value.h"

#include <stdlib.h>

ObjStruct* newStruct() {
    ObjStruct* structure = (ObjStruct*)malloc(sizeof(ObjStruct));
    initHashTable(&structure->fields);
    return structure;
}

void freeStruct(ObjStruct* structure) {
    freeHashTable(&structure->fields);
    free(structure);
}