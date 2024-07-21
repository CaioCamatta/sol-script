#ifndef sol_script_tree_parser_h
#define sol_script_tree_parser_h

#include "array.h"
#include "error.h"
#include "token.h"

// These are the types of syntax nodes SolScript has. We use it
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
 * @param previous The token just consumed.
 * @param source Root of the AST.
 * @param errors Errors that occurred during parsing.
 * */
typedef struct {
    TokenArray tokenArray;
    Token* current;
    Token* previous;
    Source* source;
    ErrorArray errors;
} ASTParser;

/**
 * Turn an array of tokens into a AST.
 *
 * Details: May throw parsing errors.
 *
 * @param treeParser an initialized ASTParser to use for scanning.
 * @return the Source of the parsed AST.
 */
Source* parseAST(ASTParser* treeParser);

/**
 * Reset the parser and parse array of tokens.
 *
 * Details: Frees the input token before returning. MayThrows parsing errors.
 *
 * @param tokenArray an initialized TokenArray to use for scanning.
 * @return the Source of the parsed AST.
 */
Source* parseASTFromTokens(ASTParser* treeParser, TokenArray* tokenArray);

/**
 * Free memory allocated for the Source of an AST.
 *
 * @param source the Source to free.
 */
void freeSource(Source* source);

/**
 *  Initialize treeParser at the beginning of the given token array to be parsed.
 *
 * @param treeParser an uninitialized ASTParser.
 * @param tokens the token array to parse.
 */
void initASTParser(ASTParser* treeParser, const TokenArray tokens);

#endif