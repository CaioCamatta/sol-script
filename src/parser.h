#ifndef delta_tree_parser_h
#define delta_tree_parser_h

#include "array.h"
#include "token.h"

// These are the types of syntax nodes Delta has. We use it
typedef enum {
    STATEMENT,
    EXPRESSION,
    LITERAL
} SyntaxType;

/**
 * ASTParser struct to facilitate the creation of a parse tree.
 *
 * @param tokenArray Array of tokens generated from the scanner.
 * @param current Current token. 0-indexed.
 * */
typedef struct {
    // Inputs from scanner
    TokenArray tokenArray;
    Token* current;

    // Root of the AST
    Source* source;

    // Track current node as we build the AST
    SyntaxType currSyntaxType;
    union {
        Statement currStatement;
        Expression currExpression;
        Literal currLiteral;
    } currNode;
} ASTParser;

/**
 * Turn an array of tokens into a AST. Returns the Source of the AST.
 *
 * Throws parsing errors.
 *
 * @param treeParser an initialized ASTParser to use for scanning.
 */
Source* parseAST(ASTParser* treeParser);

/* Initialize treeParser at the beginning of the given token array */
void initASTParser(ASTParser* treeParser, const TokenArray tokens);

#endif