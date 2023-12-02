#include "parser.h"

#include <stdbool.h>

#include "syntax.h"

// ---------------------------------------------------------------------------
// ------------------------- PARSER HELPER FUNCTIONS -------------------------
// ---------------------------------------------------------------------------

void initTreeParser(TreeParser* treeParser, const TokenArray tokens) {
    treeParser->current = tokens.values;
    treeParser->tokenArray = tokens;
}

// Move the current token to the next one.
static void advance(TreeParser* treeParser) {
    if (treeParser->current->type != TOKEN_EOF)
        treeParser->current++;
}

// Consume provided type & advance, or error
static void consume(TreeParser* treeParser, TokenType type, const char* message) {
    if (treeParser->current->type == type) {
        advance(treeParser);
        return;
    }

    errorAtCurrent(message);
}

/* Check if current token is of a given type. */
static bool check(TreeParser* treeParser, TokenType type) {
    return treeParser->current->type == type;
}

/* Consume current token if it's of a given type. Returns true if a token was consumed and false otherwise. */
static bool match(TreeParser* treeParser, TokenType type) {
    if (check(treeParser, type)) {
        advance(treeParser);
        return true;
    }
    return false;
}

/**
 * Check if an expression corresponding to a grammar production is used / is true. If so, allow
 * side effects to happen (i.e., allow the tokens used to be consumed). If not, reset the
 * `TreeParser` `current` pointer to where it was before the `expr` was evaluated.
 *
 * This is macro is necessary the `expr` evaluation has the side effect of moving the
 * `current` pointer.
 *
 * Args
 *  treeParser: TreeParser*
 *  expr: a C expression
 *      e.g., match(treeParser, TOKEN_VAL) && identifier(treeParser)
 */
#define commitIfProductionIsUsed(treeParser, expr)                  \
    do {                                                            \
        Token* currentTokenBeforeTransaction = treeParser->current; \
        if (!(expr)) {                                              \
            treeParser->current = currentTokenBeforeTransaction;    \
            return false;                                           \
        }                                                           \
        return true;                                                \
    } while (0)

// ---------------------------------------------------------------------------
// ------------------------------- PRODUCTIONS -------------------------------
// ---------------------------------------------------------------------------

/**
 * Match identifier token.
 *
 * Returns true if this production was used, false otherwise.
 */
static bool identifier(TreeParser* treeParser) {
    match(treeParser->current, TOKEN_IDENTIFIER);
}

/**
 * expression:
 *  struct-expression
 *  function-expression
 *  block-expression
 *  assignment-expression
 *
 * Returns true if this production was used, false otherwise.
 */
static bool valDeclaration(TreeParser* treeParser) {
    commitIfProductionIsUsed(treeParser, match(treeParser, TOKEN_VAL) && identifier(treeParser) && match(treeParser, TOKEN_EQUAL) && expression());
}

/**
 * val-declaration:
 *  "val" identifier ( "=" expression )?
 */
static bool valDeclaration(TreeParser* treeParser) {
    commitIfProductionIsUsed(treeParser, match(treeParser, TOKEN_VAL) && identifier(treeParser) && match(treeParser, TOKEN_EQUAL) && expression());
}

/**
 * declaration:
 *  val-declaration ";"
 *  val-declaration ";"
 *
 * Returns true if this production was used, false otherwise.
 */
static bool declaration(TreeParser* treeParser) {
    return true;
}

/**
 * expression-statement:
 *  expression ";"
 *
 * Returns true if this production was used, false otherwise.
 */
static bool expressionStatement(TreeParser* treeparser) {
    return true;
}

/**
 * statement:
 *  declaration
 *  block-statement
 *  iteration-statement
 *  selection-statement
 *  return-statement
 *  expression-statement
 */
static void statement(TreeParser* treeParser) {
    // Short-circuit
    declaration(treeParser) || expressionStatement(treeParser);
}

/**
 * Entry point for a program.
 *
 * source:
 *  statement* EOF
 */
static void source(TreeParser* treeParser) {
    while (treeParser->current != TOKEN_EOF) {
        statement(treeParser);
    }
}

TokenArray parseTree(TreeParser* treeParser) {
    source(treeParser);
}
