#ifndef delta_tree_parser_h
#define delta_tree_parser_h

#include "array.h"
#include "token.h"

/**
 * TreeParser struct to facilitate the creation of a parse tree.
 *
 * @param tokenArray Array of tokens generated from the scanner.
 * @param current Current token. 0-indexed.
 * */
typedef struct {
    TokenArray tokenArray;
    Token* current;
} TreeParser;

/**
 * Turn an array of tokens into a parse tree.
 *
 * Throws parsing errors.
 *
 * @param treeParser an initialized TreeParser to use for scanning.
 */
TokenArray parseTree(TreeParser* treeParser);

/* Initialize treeParser at the beginning of the given token array */
void initTreeParser(TreeParser* treeParser, const TokenArray tokens);

#endif