#include "debug.h"

#include <stdio.h>

void printBytecodeArray(BytecodeArray bytecodeArray) {
    printf("Compiled bytecode:\n\n");

    for (int i = 0; i < bytecodeArray.used; i++) {
        switch (bytecodeArray.values[i].type) {
            case OP_ADD:
                printf("[ ADD ]\n");
            case OP_CONSTANT:
                printf("[ CONSTANT %f ]\n", bytecodeArray.values[i].operands.constant.as.doubleVal);
            case OP_PRINT:
                printf("[ PRINT %f ]\n");
        }
    }
}