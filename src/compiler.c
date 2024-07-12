#include "compiler.h"

#include <limits.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "bytecode.h"
#include "config.h"
#include "debug.h"
#include "parser.h"
#include "syntax.h"
#include "util/hash_table.h"
#include "vm.h"

/**
 * Get new PredictedStack with null values and stack height zero.
 */
static PredictedStack newPredictedStack() {
    PredictedStack predictedStack;
    predictedStack.currentStackHeight = 0;

    for (int i = 0; i < STACK_MAX; i++) {
        predictedStack.tempStack[i].name = NULL;
    }
    return predictedStack;
}

/**
 * Initialize a CompilerUnit to compile a single unit of code, such as a function.
 * Each compiler unit contains its own bytecode array and constant pool.
 */
CompilerUnit initCompilerUnit(CompilerUnit* maybeEnclosingCompilerUnit, HashTable* globals) {
    CompilerUnit compilerUnit;

    INIT_ARRAY(compilerUnit.compiledCodeObject.bytecodeArray, Bytecode);
    INIT_ARRAY(compilerUnit.compiledCodeObject.constantPool, Constant);
    compilerUnit.predictedStack = newPredictedStack();
    compilerUnit.isInGlobalScope = maybeEnclosingCompilerUnit == NULL;  // Only the root compiler can be in global scope.
    compilerUnit.globals = globals;
    compilerUnit.enclosingCompilerUnit = maybeEnclosingCompilerUnit;

    return compilerUnit;
}

void initCompilerState(CompilerState* compilerState, Source* ASTSource) {
    compilerState->ASTSource = ASTSource;
    initHashTable(&compilerState->globals);
    compilerState->currentCompilerUnit = initCompilerUnit(NULL, &compilerState->globals);
}

void freeCompilerUnit(CompilerUnit compilerUnit) {
    // First we free the constants
    for (size_t i = 0; i < compilerUnit.compiledCodeObject.constantPool.used; i++) {
        Constant* constant = &compilerUnit.compiledCodeObject.constantPool.values[i];
        if (constant->type == CONST_TYPE_LAMBDA) {
            free(constant->as.function->code);
            free(constant->as.function);
        }
    }
    FREE_ARRAY(compilerUnit.compiledCodeObject.constantPool);

    FREE_ARRAY(compilerUnit.compiledCodeObject.bytecodeArray);
}

void freeCompilerState(CompilerState* compilerState) {
    freeCompilerUnit(compilerState->currentCompilerUnit);
}

/* FORWARD DECLARATIONS */

static void visitStatement(CompilerUnit* compiler, Statement* statement);
static void visitExpression(CompilerUnit* compiler, Expression* expression);
static void visitLiteral(CompilerUnit* compiler, Literal* literal);

/* UTILITIES */

/**
 * Print error and crash.
 *
 * TODO: make it so we don't crash completely. It should be simple to print and error, move to compiling the next statement,
 * then after all statements have been compiled print all errors and exit the program.
 */
#if DEBUG_COMPILER
#define errorAndExit(...)                                                                                                          \
    {                                                                                                                              \
        char pointerToObject[16];                                                                                                  \
        sprintf(pointerToObject, "%p", compiler);                                                                                  \
        printCompiledCodeObject(compiler->compiledCodeObject, compiler->enclosingCompilerUnit == NULL ? "main" : pointerToObject); \
        fprintf(stderr, __VA_ARGS__);                                                                                              \
        exit(EXIT_FAILURE);                                                                                                        \
    }
#else
#define errorAndExit(...)             \
    {                                 \
        fprintf(stderr, __VA_ARGS__); \
        exit(EXIT_FAILURE);           \
    }
#endif

// Add bytecode to the compiled program.
static void emitBytecode(CompilerUnit* compiler, Bytecode bytecode) {
    INSERT_ARRAY(compiler->compiledCodeObject.bytecodeArray, bytecode, Bytecode);
}

/**
 * The compiler can know in advance how tall the VM stack will be at any point. This function helps keep track
 * of the height. For example, a local variable declaration increases the stack by 1 (locals live on the stack).
 */
static void increaseStackHeight(CompilerUnit* compiler) {
    if (compiler->predictedStack.currentStackHeight == UCHAR_MAX) {
        errorAndExit(
            "StackOverflowError: The Compiler has predicted that this code will "
            "cause the VM stack to overflow.");  // This error often indicates that something is wrong
                                                 // with how the compiler tracks the predicted stack height
    }

    compiler->predictedStack.currentStackHeight++;
}

/**
 * The compiler can know in advance how tall the VM stack will be at any point. This function helps keep track
 * of the height. For example, an addition decreases the height of the stack by 1 (pop, pop, push).
 *
 * When reducing the stack height, this function also clears the slots the were freed.
 * Example:
 *  Stack: [A B C D(top) ? ?]
 *  decreaseStackHeight(1)
 *  Stack: [A B C(top) ? ? ?]
 */
static void decreaseStackHeight(CompilerUnit* compiler) {
    if (compiler->predictedStack.currentStackHeight > 0) {
        compiler->predictedStack.tempStack[compiler->predictedStack.currentStackHeight - 1].name = NULL;
        compiler->predictedStack.currentStackHeight--;
    } else
        errorAndExit("InvalidStateException: the Compiler attempted to decrease the predicted stack height below 0.")
}

static double tokenTodouble(Token token) {
    char substring[token.length + 1];
    memcpy(substring, token.start, token.length);
    substring[token.length] = '\0';
    return atof(token.start);
}

// Copy a string from the input code to the heap and make it null-terminated so it can be used by the VM.
char* copyStringToHeap(const char* chars, int length) {
    // TODO: consider adding the hash here to potentially save time in the VM
    char* heapString = malloc(sizeof(char) * (length + 1));
    memcpy(heapString, chars, length);
    heapString[length] = '\0';
    return heapString;
}

/**
 * Given a new constant, try to find its index in the constant pool. Returns -1 if not in pool or the index.
 *
 * TODO: this method is terribly inefficient as it iterates through the entire table. We should keep a hash table
 * of (number -> index in table) on the side and discard it after compilation is done.
 */
static size_t findConstantInPool(CompilerUnit* compiler, Constant constant) {
    for (size_t i = 0; i < compiler->compiledCodeObject.constantPool.used; i++) {
        // Skip if its not the same type
        if (constant.type != compiler->compiledCodeObject.constantPool.values[i].type) continue;

        // Check if value is the same
        switch (compiler->compiledCodeObject.constantPool.values[i].type) {
            case CONST_TYPE_STRING:
            case CONST_TYPE_IDENTIFIER:
                if (strcmp(constant.as.string, compiler->compiledCodeObject.constantPool.values[i].as.string) == 0) return i;
                break;
            case CONST_TYPE_DOUBLE:
                if (constant.as.number == compiler->compiledCodeObject.constantPool.values[i].as.number) return i;
                break;
            case CONST_TYPE_LAMBDA:
                if (&constant.as.function == &compiler->compiledCodeObject.constantPool.values[i].as.function) return i;
                break;
        }
    }
    return -1;
}

/**
 * Add a constant to the compiler's constant pool, returns index in the pool.
 * If the constant is already in the pool, the index of the existing one is returned.
 * (These constants go alongwise the bytecode in the compiled code.)
 * */
static size_t addConstantToPool(CompilerUnit* compiler, Constant constant) {
    // Check if its already there
    size_t maybeIndexInPool = findConstantInPool(compiler, constant);
    if (maybeIndexInPool != -1) return maybeIndexInPool;

    // If not, insert and return index
    INSERT_ARRAY(compiler->compiledCodeObject.constantPool, constant, Constant);
    return compiler->compiledCodeObject.constantPool.used - 1;
}

/**
 * Add a global variable to the compiler's table of globals.
 */
static void addGlobalToTable(CompilerUnit* compiler, char* name, bool isModifiable) {
    hashTableInsert(compiler->globals, name, (Value){.as.booleanVal = isModifiable});
}
/**
 * Check if a global variable in the compiler's table of globals is constant (i.e. `val`).
 * Throws an error if the global doesn't exist.
 */
static bool isGlobalInTable(CompilerUnit* compiler, char* name) {
    HashTableEntry* entry = hashTableGet(compiler->globals, name);
    return entry->key != NULL;
}

/**
 * Check if a global variable in the compiler's table of globals is constant (i.e. `val`).
 * Throws an error if the global doesn't exist.
 */
static bool isGlobalConstant(CompilerUnit* compiler, char* name) {
    HashTableEntry* entry = hashTableGet(compiler->globals, name);
    if (entry) {
        return entry->value.as.booleanVal;
    } else {
        errorAndExit(
            "InvalidStateException: Attempted to check if a global variable is constant, but the "
            "global doesn't exist in the Compiler's hash table.")
    }
}

/**
 * Given a local variable name, find its position in the stack. (We can determine at compile time
 * where that local will be in the stack.)
 *
 * Returns either the stack index if found, or -1 if not found.
 */
static int findLocalByName(CompilerUnit* compiler, char* name) {
    for (int i = compiler->predictedStack.currentStackHeight; i >= 0; i--) {
        // Skip NULL entries. (These may exists because our temp stack only keeps track of locals.
        // Anything else that's on the stack is a NULL here)
        if (compiler->predictedStack.tempStack[i].name == NULL) continue;

        // See if name matches
        if (strcmp(name, compiler->predictedStack.tempStack[i].name) == 0) return i;
    }
    return -1;  // Not declared
}

/**
 * Check if a local variable in the compiler's table of locals is constant (i.e. `val`).
 * Throws an error if the local doesn't exist.
 */
static int isLocalConstant(CompilerUnit* compiler, char* name) {
    int index = findLocalByName(compiler, name);
    if (index == -1)
        errorAndExit(
            "InvalidStateException: Attempted to check if a local variable is constant, but the "
            "local doesn't exist.") else {
            return compiler->predictedStack.tempStack[index].isModifiable;
        }
}
/**
 * Check if a local variable in the compiler's table of locals is constant (i.e. `val`).
 * Throws an error if the local doesn't exist.
 */
static int isLocalConstantByIndex(CompilerUnit* compiler, int indexInTempStack) {
    if (indexInTempStack == -1)
        errorAndExit(
            "InvalidStateException: Attempted to check if a local variable is constant, but the "
            "local doesn't exist.") else {
            return compiler->predictedStack.tempStack[indexInTempStack].isModifiable;
        }
}

/**
 * Add a local variable to the Compiler's temporary stack..
 */
static void addLocalToTempStack(CompilerUnit* compiler, char* name, bool isModifiable) {
    // The local will be right below the current stack height.
    // It's assumed that, at this point, the stack height has already been incremented.
    compiler->predictedStack.tempStack[compiler->predictedStack.currentStackHeight - 1] = (Local){.name = name, .isModifiable = isModifiable};
}

/**
 * Remove N locals from the top of the Compiler's temporary stack.
 */
static void removeLocalsFromTempStack(CompilerUnit* compiler, int N) {
    for (int i = compiler->predictedStack.currentStackHeight; i > compiler->predictedStack.currentStackHeight - N; i--) {
        if (N < 0) {
            errorAndExit("Attempted to remove more local variables than exist in the stack. This should be impossible.");
        }
        compiler->predictedStack.tempStack[i - 1].name = NULL;  // Setting the name to NULL frees up the spot
    }
}

/* VISITOR FUNCTIONS */

static void visitAdditiveExpression(CompilerUnit* compiler, AdditiveExpression* additiveExpression) {
    visitExpression(compiler, additiveExpression->leftExpression);
    visitExpression(compiler, additiveExpression->rightExpression);

    switch (additiveExpression->punctuator.type) {
        case TOKEN_PLUS:
            emitBytecode(compiler, BYTECODE(OP_BINARY_ADD));
            break;
        case TOKEN_MINUS:
            emitBytecode(compiler, BYTECODE(OP_BINARY_SUBTRACT));
            break;
        default:
            break;
    }
    decreaseStackHeight(compiler);
}

static void visitMultiplicativeExpression(CompilerUnit* compiler, MultiplicativeExpression* multiplicativeExpression) {
    visitExpression(compiler, multiplicativeExpression->leftExpression);
    visitExpression(compiler, multiplicativeExpression->rightExpression);

    switch (multiplicativeExpression->punctuator.type) {
        case TOKEN_STAR:
            emitBytecode(compiler, BYTECODE(OP_BINARY_MULTIPLY));
            break;
        case TOKEN_SLASH:
            emitBytecode(compiler, BYTECODE(OP_BINARY_DIVIDE));
            break;
        default:
            break;
    }
    decreaseStackHeight(compiler);
}

static void visitEqualityExpression(CompilerUnit* compiler, EqualityExpression* equalityExpression) {
    visitExpression(compiler, equalityExpression->leftExpression);
    visitExpression(compiler, equalityExpression->rightExpression);

    switch (equalityExpression->punctuator.type) {
        case TOKEN_EQUAL_EQUAL:
            emitBytecode(compiler, BYTECODE(OP_BINARY_EQUAL));
            break;
        case TOKEN_EXCLAMATION_EQUAL:
            emitBytecode(compiler, BYTECODE(OP_BINARY_NOT_EQUAL));
            break;
        default:
            break;
    }
    decreaseStackHeight(compiler);
}

static void visitLogicalOrExpression(CompilerUnit* compiler, LogicalOrExpression* logicalOrExpression) {
    visitExpression(compiler, logicalOrExpression->leftExpression);
    visitExpression(compiler, logicalOrExpression->rightExpression);
    emitBytecode(compiler, BYTECODE(OP_BINARY_LOGICAL_OR));
    decreaseStackHeight(compiler);
}

static void visitLogicalAndExpression(CompilerUnit* compiler, LogicalAndExpression* logicalAndExpression) {
    visitExpression(compiler, logicalAndExpression->leftExpression);
    visitExpression(compiler, logicalAndExpression->rightExpression);
    emitBytecode(compiler, BYTECODE(OP_BINARY_LOGICAL_AND));
    decreaseStackHeight(compiler);
}

// Visit the two expression on left and write, emit bytecode to add them
static void visitComparisonExpression(CompilerUnit* compiler, ComparisonExpression* comparisonExpression) {
    visitExpression(compiler, comparisonExpression->leftExpression);
    visitExpression(compiler, comparisonExpression->rightExpression);

    switch (comparisonExpression->punctuator.type) {
        case TOKEN_GREATER:
            emitBytecode(compiler, BYTECODE(OP_BINARY_GT));
            break;
        case TOKEN_GREATER_EQUAL:
            emitBytecode(compiler, BYTECODE(OP_BINARY_GTE));
            break;
        case TOKEN_LESSER:
            emitBytecode(compiler, BYTECODE(OP_BINARY_LT));
            break;
        case TOKEN_LESSER_EQUAL:
            emitBytecode(compiler, BYTECODE(OP_BINARY_LTE));
            break;
        default:
            break;
    }
    decreaseStackHeight(compiler);
}

static void visitUnaryExpression(CompilerUnit* compiler, UnaryExpression* unaryExpression) {
    visitExpression(compiler, unaryExpression->rightExpression);
    if (unaryExpression->punctuator.type == TOKEN_MINUS) {
        emitBytecode(compiler, BYTECODE(OP_UNARY_NEGATE));
    } else if (unaryExpression->punctuator.type == TOKEN_EXCLAMATION) {
        emitBytecode(compiler, BYTECODE(OP_UNARY_NOT));
    }
}

static void visitPrimaryExpression(CompilerUnit* compiler, PrimaryExpression* primaryExpression) {
    visitLiteral(compiler, primaryExpression->literal);
}

// Visit the expression. (Should be executed only for its side-effects)
static void visitExpressionStatement(CompilerUnit* compiler, ExpressionStatement* expressionStatement) {
    visitExpression(compiler, expressionStatement->expression);

    // The expression will always add a Value to the stack, but by definition it's not used
    // (otherwise it wouldn't be an expression statement) so we need to remove it.
    emitBytecode(compiler, BYTECODE_OPERAND_1(OP_POPN, 1));
    decreaseStackHeight(compiler);
}

static void visitAssignmentStatement(CompilerUnit* compiler, AssignmentStatement* assignmentStatement) {
    // Compile the code to compute the new value
    visitExpression(compiler, assignmentStatement->value);

    // We don't need to visit the target expression.
    // visitExpression(compiler, assignmentStatement->target);

    // Emit bytecode based on the target type
    if (assignmentStatement->target->type == PRIMARY_EXPRESSION) {
        PrimaryExpression* primaryExpr = assignmentStatement->target->as.primaryExpression;
        if (primaryExpr->literal->type == IDENTIFIER_LITERAL) {
            IdentifierLiteral* identifierLiteral = primaryExpr->literal->as.identifierLiteral;
            char* identifierName = strndup(identifierLiteral->token.start, identifierLiteral->token.length);

            // Check if its a local variable
            size_t stackIndex = findLocalByName(compiler, identifierName);

            if (stackIndex == -1) {  // Not a local variable
                if (isGlobalInTable(compiler, identifierName)) {
                    if (isGlobalConstant(compiler, identifierName)) {
                        errorAndExit("Error: Cannot modify global constant '%s'.", identifierName);
                    }

                    // Global variable assignment
                    Constant constant = IDENTIFIER_CONST(identifierName);
                    size_t constantIndex = addConstantToPool(compiler, constant);
                    emitBytecode(compiler, BYTECODE_OPERAND_1(OP_SET_GLOBAL_VAR, constantIndex));
                } else {
                    errorAndExit("Error: identifier '%s' not declared.", identifierName);
                }
            } else {
                if (isLocalConstantByIndex(compiler, stackIndex)) {
                    errorAndExit("Error: Cannot modify local constant '%s'.", identifierName);
                }

                if (stackIndex != -1) {
                    emitBytecode(compiler, BYTECODE_OPERAND_1(OP_SET_LOCAL_VAR_FAST, stackIndex));
                } else {
                    errorAndExit("Error: identifier '%s' not declared.", identifierName);
                }
            }
        } else {
            errorAndExit("Invalid assignment target. Invalid type of primary-expression.");
        }
    } else {
        errorAndExit("Invalid assignment target.");
    }

    // The new value is already popped by the `OP_SET_*` operation, so we just need to update the stack height.
    decreaseStackHeight(compiler);
}

/**
 * Visit the expression after the val declaration, save the variable identifier to constant pool,
 * emit instruction to set identifier = val
 */
static void visitValDeclarationStatement(CompilerUnit* compiler, ValDeclarationStatement* valDeclarationStatement) {
    visitExpression(compiler, valDeclarationStatement->expression);
    // The expression will add 1 to the stack height. We leave the value on the stack - that's the variable.

    Constant constant = IDENTIFIER_CONST(copyStringToHeap(valDeclarationStatement->identifier->token.start,
                                                          valDeclarationStatement->identifier->token.length));

    bool isVariableModifiable = true;  // Vals cannot be modified

    if (compiler->isInGlobalScope) {
        if (isGlobalInTable(compiler, constant.as.string)) errorAndExit("Error: val \"%s\" is already declared. Redeclaration is not permitted.", constant.as.string);

        size_t constantIndex = addConstantToPool(compiler, constant);
        addGlobalToTable(compiler, constant.as.string, isVariableModifiable);  // We also keep track that this global exists so we can prevent redeclaration later.

        emitBytecode(compiler, BYTECODE_OPERAND_1(OP_DEFINE_GLOBAL_VAL, constantIndex));

        decreaseStackHeight(compiler);  // Globals are popped from the stack.
    } else {
        if (findLocalByName(compiler, constant.as.string) != -1) errorAndExit("Error: val \"%s\" is already declared locally. Redeclaration is not permitted.", constant.as.string);

        addLocalToTempStack(compiler, constant.as.string, isVariableModifiable);
        emitBytecode(compiler, BYTECODE(OP_DEFINE_LOCAL_VAL_FAST));
    }
}

static void visitVarDeclarationStatement(CompilerUnit* compiler, VarDeclarationStatement* varDeclarationStatement) {
    bool isValueNull = varDeclarationStatement->maybeExpression == NULL;
    if (!isValueNull)
        visitExpression(compiler, varDeclarationStatement->maybeExpression);
    else {
        emitBytecode(compiler, BYTECODE(OP_NULL));
        increaseStackHeight(compiler);
    }

    Constant constant = IDENTIFIER_CONST(copyStringToHeap(varDeclarationStatement->identifier->token.start,
                                                          varDeclarationStatement->identifier->token.length));

    bool isVariableModifiable = false;  // Vars can be modified

    if (compiler->isInGlobalScope) {
        // Preventing global redeclaration is an arbitrary choice.
        if (isGlobalInTable(compiler, constant.as.string)) errorAndExit("Error: var \"%s\" is already declared. Redeclaration is not permitted as it often leads to confusion.", constant.as.string);

        size_t constantIndex = addConstantToPool(compiler, constant);
        addGlobalToTable(compiler, constant.as.string, isVariableModifiable);  // We also keep track that this global exists so we can prevent redeclaration later.

        emitBytecode(compiler, BYTECODE_OPERAND_1(OP_DEFINE_GLOBAL_VAR, constantIndex));

        decreaseStackHeight(compiler);  // The value assigned to the Global is popped from the stack after we set the Global.
    } else {
        if (findLocalByName(compiler, constant.as.string) != -1) errorAndExit("Error: var \"%s\" is already declared locally. Redeclaration is not permitted.", constant.as.string);

        addLocalToTempStack(compiler, constant.as.string, isVariableModifiable);

        emitBytecode(compiler, BYTECODE(OP_DEFINE_LOCAL_VAR_FAST));
    }
}

// Visit expression following print, then emit bytecode to print that expression
static void visitPrintStatement(CompilerUnit* compiler, PrintStatement* printStatement) {
    visitExpression(compiler, printStatement->expression);
    emitBytecode(compiler, BYTECODE(OP_PRINT));
    decreaseStackHeight(compiler);
}

static void visitBlockExpression(CompilerUnit* compiler, BlockExpression* blockExpression) {
    bool wasCompilerInGlobalScopeBeforeThisBlock = compiler->isInGlobalScope;
    compiler->isInGlobalScope = false;

    // Keep track of the stack height so we can later pop all the variables etc defined in it.
    uint8_t stackHeightBeforeBlockStmt = compiler->predictedStack.currentStackHeight;

    for (size_t i = 0; i < blockExpression->statementArray.used; i++) {
        Statement* statement = blockExpression->statementArray.values[i];
        visitStatement(compiler, statement);
    }

    // If there's a final expression, visit it. Otherwise emit a NULL.
    if (blockExpression->lastExpression) {
        visitExpression(compiler, blockExpression->lastExpression);
    } else {
        emitBytecode(compiler, BYTECODE(OP_NULL));
        increaseStackHeight(compiler);
    }

    // Calculate the stack effect of this entire block so we can clean up at the end of the block.
    uint8_t stackHeightAfterBlockStmt = compiler->predictedStack.currentStackHeight;
    uint8_t blockStmtStackEffect = stackHeightAfterBlockStmt - stackHeightBeforeBlockStmt;

    // Swap the value at the top of the stack (the Value produced by the expression) with the first value
    // produced in the block. Example:
    // Before: [ X X X BlockValue_0 BlockValue_1 BlockValue_2]
    // Apply: SWAP(2)
    // After:  [ X X X BlockValue_2 BlockValue_1 BlockValue_0]
    if (blockStmtStackEffect > 1)
        emitBytecode(compiler, BYTECODE_OPERAND_1(OP_SWAP, blockStmtStackEffect - 1));

    // Pop all the Values that were put in the VM stack in the block, except the Value produced by the expression
    // TODO: optimization; stop emitting POP when blockStmtStackEffect = 0.
    emitBytecode(compiler, BYTECODE_OPERAND_1(OP_POPN, blockStmtStackEffect - 1));

    // Pop all the Locals that were put in the Compiler stack in the block, except for the final expression.
    removeLocalsFromTempStack(compiler, blockStmtStackEffect - 1);

    // Undo stack height, except for final expression
    compiler->predictedStack.currentStackHeight = stackHeightBeforeBlockStmt + 1;

    compiler->isInGlobalScope = wasCompilerInGlobalScopeBeforeThisBlock;
}

static void visitBlockStatement(CompilerUnit* compiler, BlockStatement* blockStatement) {
    bool wasCompilerInGlobalScopeBeforeThisBlock = compiler->isInGlobalScope;
    compiler->isInGlobalScope = false;

    // Keep track of the stack height so we can later pop all the variables etc defined in it.
    uint8_t stackHeightBeforeBlockStmt = compiler->predictedStack.currentStackHeight;

    for (size_t i = 0; i < blockStatement->statementArray.used; i++) {
        Statement* statement = blockStatement->statementArray.values[i];
        visitStatement(compiler, statement);
    }

    // Calculate the stack effect of this entire block so we can clean up at the end of the block.
    uint8_t stackHeightAfterBlockStmt = compiler->predictedStack.currentStackHeight;
    uint8_t blockStmtStackEffect = stackHeightAfterBlockStmt - stackHeightBeforeBlockStmt;

    // Pop all the Values that were put in the VM stack in the block.
    // TODO: optimization; stop emitting POP when blockStmtStackEffect = 0.
    emitBytecode(compiler, BYTECODE_OPERAND_1(OP_POPN, blockStmtStackEffect));

    // Pop all the Locals that were put in the Compiler stack in the block.
    removeLocalsFromTempStack(compiler, blockStmtStackEffect);

    // Undo stack height
    compiler->predictedStack.currentStackHeight = stackHeightBeforeBlockStmt;

    compiler->isInGlobalScope = wasCompilerInGlobalScopeBeforeThisBlock;
}

static void visitSelectionStatement(CompilerUnit* compiler, SelectionStatement* selectionStatement) {
    // Visit the condition
    visitExpression(compiler, selectionStatement->conditionExpression);

    // Emit bytecode for conditional jump, keep track of its index in jumpIfFalsePosition.
    // 999999 is a placeholder. We don't know how much bytecode is in the statement for the "then" branch so
    // we will have to come back and patch this placeholder.
    size_t jumpIfFalsePosition = compiler->compiledCodeObject.bytecodeArray.used;
    emitBytecode(compiler, BYTECODE_OPERAND_1(OP_JUMP_IF_FALSE, 999999));

    // Visiting the expression grows the stack, but running OP_JUMP_IF_FALSE in the VM will
    // decrease it.
    decreaseStackHeight(compiler);

    // Visit the true (a.k.a. "then") branch.
    visitStatement(compiler, selectionStatement->trueStatement);

    // If there's an else branch, we need to jump over it once the true branch is executed
    size_t jumpToEndPosition = 0;
    if (selectionStatement->falseStatement != NULL) {
        jumpToEndPosition = compiler->compiledCodeObject.bytecodeArray.used;
        emitBytecode(compiler, BYTECODE_OPERAND_1(OP_JUMP, 999999));  // We'll have to patch this too
    }

    // Patch the jump-if-false position now that we know where to jump
    compiler->compiledCodeObject.bytecodeArray.values[jumpIfFalsePosition].maybeOperand1 = compiler->compiledCodeObject.bytecodeArray.used;

    // Visit the false (a.k.a. "else") branch if it exists.
    if (selectionStatement->falseStatement != NULL) {
        visitStatement(compiler, selectionStatement->falseStatement);
        // Back-patch the jump-to-end position.
        compiler->compiledCodeObject.bytecodeArray.values[jumpToEndPosition].maybeOperand1 = compiler->compiledCodeObject.bytecodeArray.used;
    }
}

static Function* createFunction(int parameterCount, CompiledCodeObject* code) {
    Function* function = (Function*)malloc(sizeof(Function));
    function->parameterCount = parameterCount;
    function->code = code;
    return function;
}

/**
 * Compile a function and emit an OP_LAMBDA, which in the just puts the function Value on the stack.
 *
 * It's impossible to predict the size of the global stack at time of invocation, the best we can do is
 * predict the size of the stack within the context of the invocation. so to compile a function we
 * create a new CompilerUnit, with its own stack, and compile everything relative to the new stack.
 *
 * TODO: Handle closures. For now, only globals and variables that are local to the function can be referenced.
 */
static void visitLambdaExpression(CompilerUnit* compiler, LambdaExpression* lambdaExpression) {
    CompilerUnit functionCompiler = initCompilerUnit(compiler, compiler->globals);

    // Define all parameters as locals
    for (size_t i = 0; i < lambdaExpression->parameters->used; i++) {
        Constant constant = IDENTIFIER_CONST(copyStringToHeap(lambdaExpression->parameters->values[i].token.start,
                                                              lambdaExpression->parameters->values[i].token.length));
        if (findLocalByName(&functionCompiler, constant.as.string) != -1)
            errorAndExit("Error: var \"%s\" is already declared locally. Redeclaration is not permitted.", constant.as.string);
        increaseStackHeight(&functionCompiler);
        addLocalToTempStack(&functionCompiler, constant.as.string, false);
    }

    // Compile the function body
    visitBlockExpression(&functionCompiler, lambdaExpression->bodyBlock);

    // Emit OP_RETURN if not already present
    if (functionCompiler.compiledCodeObject.bytecodeArray.values[functionCompiler.compiledCodeObject.bytecodeArray.used - 1].type != OP_RETURN) {
        emitBytecode(&functionCompiler, (Bytecode){.type = OP_RETURN});
    }

    // Allocate the compiled code object on the heap and create a function object
    CompiledCodeObject* heapCodeObject = (CompiledCodeObject*)malloc(sizeof(CompiledCodeObject));
    *heapCodeObject = functionCompiler.compiledCodeObject;
    Function* function = createFunction(lambdaExpression->parameters->used, heapCodeObject);

    // Create a constant for the function
    Constant functionConstant = LAMBDA_CONST(function);
    size_t constantIndex = addConstantToPool(compiler, functionConstant);

    // Emit bytecode to define the function
    emitBytecode(compiler, (Bytecode){.type = OP_LAMBDA, .maybeOperand1 = constantIndex});

    // The VM will be putting an ObjFunction in the stack, so we have to increase the height
    increaseStackHeight(compiler);
}

/**
 * Call a function.
 *  1) Put all arguments on the stack; the bottommost one is the first argument.
 *  2) Verify function exists in lexical scope
 *  3) TODO: Check that identifier being called is actually a calalble
 *  4) TODO: Check arity
 *  5) Emit bytecode for put function object (referenced by the identifier) being caled on the stack
 *  6) Emit bytecode to execute the function object
 */
static void visitCallExpression(CompilerUnit* compiler, CallExpression* callExpression) {
    // Compile the arguments
    for (size_t i = 0; i < callExpression->arguments->used; i++) {
        visitExpression(compiler, callExpression->arguments->values[i]);
    }

    // Verify that the identifier being called exists in the lexical scope
    char* identifierNameNullTerminated = strndup(callExpression->lambdaFunctionName->token.start, callExpression->lambdaFunctionName->token.length);
    size_t functionIndex = findLocalByName(compiler, identifierNameNullTerminated);
    Constant functionNameConstant = IDENTIFIER_CONST(identifierNameNullTerminated);

    if (functionIndex == -1) {
        // It's not a local variable, so we check if it's a global
        size_t index = findConstantInPool(compiler, functionNameConstant);
        if (!isGlobalInTable(compiler, functionNameConstant.as.string)) errorAndExit("Error: identifier '%s' referenced before declaration.", functionNameConstant.as.string);

        // Emit bytecode to load the function object onto the stack
        Bytecode bytecodeToGetVariable = isGlobalConstant(compiler, functionNameConstant.as.string)
                                             ? BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAL, index)
                                             : BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAR, index);
        emitBytecode(compiler, bytecodeToGetVariable);
    } else {
        Bytecode bytecodeToGetVariable = isLocalConstant(compiler, functionNameConstant.as.string)
                                             ? BYTECODE_OPERAND_1(OP_GET_LOCAL_VAL_FAST, functionIndex)
                                             : BYTECODE_OPERAND_1(OP_GET_LOCAL_VAR_FAST, functionIndex);
        emitBytecode(compiler, bytecodeToGetVariable);
    }

    // TODO: Add compile-time type check. Only functions should be callable. (This could be done by adding
    // a type field to the Local struct)

    // TODO: Add arity check.

    // Emit bytecode for the function call
    emitBytecode(compiler, BYTECODE(OP_CALL));
}

/**
 * Here's an example of the bytecode produced by SolScript while-loops:
 *  42    : loop condition expression
 *  43    : if condition expression is falsey, skip the body of the loop (go to 68)
 *  44-66 : loop body
 *  67    : jump to condition expression (42)
 *  68    : ...
 *
 * TODO: benchmark having condition at the end of the loop and doing a jump-if-true to start
 */
static void visitIterationStatement(CompilerUnit* compiler, IterationStatement* iterationStatement) {
    // Keep a pointer to the condition expression so we can jump to it later
    size_t expressionInstructionPosition = compiler->compiledCodeObject.bytecodeArray.used;

    // Compile the actual expression
    visitExpression(compiler, iterationStatement->conditionExpression);

    // In the VM, if the expression is falsey, we need to jump past the body of the loop.
    // '999999' is a placeholder; at this point we don't know how much bytecode the body will
    // emit, so we need to use placeholder and patch it later.
    size_t jumpIfFalsePosition = compiler->compiledCodeObject.bytecodeArray.used;
    emitBytecode(compiler, BYTECODE_OPERAND_1(OP_JUMP_IF_FALSE, 999999));

    // Visiting the expression grows the stack, but running OP_JUMP_IF_FALSE in the VM will
    // decrease it.
    decreaseStackHeight(compiler);

    // Visit the body of the loop
    visitStatement(compiler, iterationStatement->bodyStatement);

    // Jump to condition expression so we can re-evaluate it
    emitBytecode(compiler, BYTECODE_OPERAND_1(OP_JUMP, expressionInstructionPosition));

    // Now that we've compiled the body, we can patch the jump-if-false instruction
    compiler->compiledCodeObject.bytecodeArray.values[jumpIfFalsePosition].maybeOperand1 = compiler->compiledCodeObject.bytecodeArray.used;
}

// Add number to constant pool and emit bytecode to load it onto the stack
static void visitNumberLiteral(CompilerUnit* compiler, NumberLiteral* numberLiteral) {
    double number = tokenTodouble(numberLiteral->token);
    Constant constant = DOUBLE_CONST(number);
    size_t constantIndex = addConstantToPool(compiler, constant);

    Bytecode bytecode = BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, constantIndex);
    emitBytecode(compiler, bytecode);

    increaseStackHeight(compiler);
}

static void visitBooleanLiteral(CompilerUnit* compiler, BooleanLiteral* booleanLiteral) {
    switch (booleanLiteral->token.type) {
        case TOKEN_FALSE:
            emitBytecode(compiler, BYTECODE(OP_FALSE));
            break;
        case TOKEN_TRUE:
            emitBytecode(compiler, BYTECODE(OP_TRUE));
            break;
        default:
            errorAndExit("Error: failed to parse boolean from token '%.*s'.", booleanLiteral->token.length, booleanLiteral->token.start);
            break;
    }
    increaseStackHeight(compiler);
}

static void visitIdentifierLiteral(CompilerUnit* compiler, IdentifierLiteral* identifierLiteral) {
    // Check if the identifier is a local variable
    char* identifierNameNullTerminated = strndup(identifierLiteral->token.start, identifierLiteral->token.length);
    size_t stackIndex = findLocalByName(compiler, identifierNameNullTerminated);

    if (stackIndex == -1) {  // Its not a local variable
        Constant constant = (Constant){
            .type = CONST_TYPE_IDENTIFIER,
            .as = {identifierNameNullTerminated}};
        size_t index = findConstantInPool(compiler, constant);

        if (!isGlobalInTable(compiler, identifierNameNullTerminated)) errorAndExit("Error: identifier '%s' referenced before declaration.", identifierNameNullTerminated);

        Bytecode bytecodeToGetVariable = isGlobalConstant(compiler, identifierNameNullTerminated)
                                             ? BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAL, index)
                                             : BYTECODE_OPERAND_1(OP_GET_GLOBAL_VAR, index);
        emitBytecode(compiler, bytecodeToGetVariable);
    } else {
        Bytecode bytecodeToGetVariable = isLocalConstant(compiler, identifierNameNullTerminated)
                                             ? BYTECODE_OPERAND_1(OP_GET_LOCAL_VAL_FAST, stackIndex)
                                             : BYTECODE_OPERAND_1(OP_GET_LOCAL_VAR_FAST, stackIndex);
        emitBytecode(compiler, bytecodeToGetVariable);
    }

    increaseStackHeight(compiler);
}

static void visitStringLiteral(CompilerUnit* compiler, StringLiteral* stringLiteral) {
    // Remove "'s from left and right of the string
    char* stringTrimmedAndNullTerminated = strndup(stringLiteral->token.start + 1, stringLiteral->token.length - 2);
    Constant constant = (Constant){
        .type = CONST_TYPE_STRING,
        .as = {stringTrimmedAndNullTerminated}};
    size_t constantIndex = addConstantToPool(compiler, constant);

    Bytecode bytecodeToGetVariable = BYTECODE_OPERAND_1(OP_LOAD_CONSTANT, constantIndex);
    emitBytecode(compiler, bytecodeToGetVariable);

    increaseStackHeight(compiler);
}

static void visitLiteral(CompilerUnit* compiler, Literal* literal) {
    switch (literal->type) {
        case NUMBER_LITERAL:
            visitNumberLiteral(compiler, literal->as.numberLiteral);
            break;
        case IDENTIFIER_LITERAL:
            visitIdentifierLiteral(compiler, literal->as.identifierLiteral);
            break;
        case STRING_LITERAL:
            visitStringLiteral(compiler, literal->as.stringLiteral);
            break;
        case BOOLEAN_LITERAL:
            visitBooleanLiteral(compiler, literal->as.booleanLiteral);
            break;
        default:
            fprintf(stderr, "Unimplemented literal type %d.", literal->type);
            exit(EXIT_FAILURE);
            break;
    }
}

static void visitExpression(CompilerUnit* compiler, Expression* expression) {
    switch (expression->type) {
        case ADDITIVE_EXPRESSION:
            visitAdditiveExpression(compiler, expression->as.additiveExpression);
            break;
        case MULTIPLICATIVE_EXPRESSION:
            visitMultiplicativeExpression(compiler, expression->as.multiplicativeExpression);
            break;
        case UNARY_EXPRESSION:
            visitUnaryExpression(compiler, expression->as.unaryExpression);
            break;
        case PRIMARY_EXPRESSION:
            visitPrimaryExpression(compiler, expression->as.primaryExpression);
            break;
        case EQUALITY_EXPRESSION:
            visitEqualityExpression(compiler, expression->as.equalityExpression);
            break;
        case LOGICAL_OR_EXPRESSION:
            visitLogicalOrExpression(compiler, expression->as.logicalOrExpression);
            break;
        case LOGICAL_AND_EXPRESSION:
            visitLogicalAndExpression(compiler, expression->as.logicalAndExpression);
            break;
        case COMPARISON_EXPRESSION:
            visitComparisonExpression(compiler, expression->as.comparisonExpression);
            break;
        case BLOCK_EXPRESSION:
            visitBlockExpression(compiler, expression->as.blockExpression);
            break;
        case LAMBDA_EXPRESSION:
            visitLambdaExpression(compiler, expression->as.lambdaExpression);
            break;
        case CALL_EXPRESSION:
            visitCallExpression(compiler, expression->as.callExpression);
            break;
        default:
            fprintf(stderr, "Unimplemented expression type %d.", expression->type);
            exit(EXIT_FAILURE);
            break;
    }
}

static void visitStatement(CompilerUnit* compiler, Statement* statement) {
    switch (statement->type) {
        case EXPRESSION_STATEMENT:
            visitExpressionStatement(compiler, statement->as.expressionStatement);
            break;
        case VAL_DECLARATION_STATEMENT:
            visitValDeclarationStatement(compiler, statement->as.valDeclarationStatement);
            break;
        case VAR_DECLARATION_STATEMENT:
            visitVarDeclarationStatement(compiler, statement->as.varDeclarationStatement);
            break;
        case ASSIGNMENT_STATEMENT:
            visitAssignmentStatement(compiler, statement->as.assignmentStatement);
            break;
        case PRINT_STATEMENT:
            visitPrintStatement(compiler, statement->as.printStatement);
            break;
        case BLOCK_STATEMENT:
            visitBlockStatement(compiler, statement->as.blockStatement);
            break;
        case SELECTION_STATEMENT:
            visitSelectionStatement(compiler, statement->as.selectionStatement);
            break;
        case ITERATION_STATEMENT:
            visitIterationStatement(compiler, statement->as.iterationStatement);
            break;
        default:
            fprintf(stderr, "Unimplemented statement type %d.\n", statement->type);
            exit(EXIT_FAILURE);
            break;
    }
}

CompiledCode compile(CompilerState* initializedRootCompilerState) {
#if DEBUG_COMPILER
    clock_t startTime = clock();
    printf("Started compiling.\n");
#endif

    for (int i = 0; i < initializedRootCompilerState->ASTSource->numberOfStatements; i++) {
        Statement* statement = initializedRootCompilerState->ASTSource->rootStatements[i];
        visitStatement(&initializedRootCompilerState->currentCompilerUnit, statement);
    }

#if DEBUG_COMPILER
    clock_t endTime = clock();
    double timeTaken = ((double)(endTime - startTime)) / CLOCKS_PER_SEC;
    printf("Done compiling in %.5f seconds.\n\n", timeTaken);
#endif

    CompiledCode code = (CompiledCode){
        .topLevelCodeObject = initializedRootCompilerState->currentCompilerUnit.compiledCodeObject};

    return code;
}