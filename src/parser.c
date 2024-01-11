#include "parser.h"

#include <stdbool.h>

#include "syntax.h"

// ---------------------------------------------------------------------------
// ------------------------- PARSER HELPER FUNCTIONS -------------------------
// ---------------------------------------------------------------------------

void initASTParser(ASTParser* parser, const TokenArray tokens) {
    parser->current = tokens.values;
    parser->tokenArray = tokens;
    parser->source = (Source*)malloc(sizeof(Source));
    parser->source->numberOfStatements = 0;
}

// Allocate an AST node (literal, expression, statement) on the heap and return a pointer to the allocated node.
#define allocateASTNode(type) (type*)malloc(sizeof(type));

// Free an AST node (literal, expression, statement) on the heap.
#define freeASTNode(node) free(node);

// Free memory allocated for the Source of an AST.
void freeSource(Source* source) {
    // TODO: add logic to make freeing recursive
    free(source);
}

// Print error at current token, halt execution
static void errorAtCurrent(ASTParser* parser, const char* message) {
    fprintf(stderr, "Error at line %d, column %d: %s", parser->current->lineNo, parser->current->colNo, message);
    exit(EXIT_FAILURE);
}

// Peek token to be immediately parsed.
static Token* peek(ASTParser* parser) {
    return parser->current;
}

// Move the current token to the next one.
static void advance(ASTParser* parser) {
    if (parser->current->type != TOKEN_EOF)
        parser->current++;
}

// Consume provided type & advance, or error
static Token* consume(ASTParser* parser, TokenType type, const char* message) {
    if (parser->current->type == type) {
        Token* currToken = parser->current;
        advance(parser);
        return currToken;
    }

    errorAtCurrent(parser, message);
}

/* Check if current token is of a given type. */
static bool check(ASTParser* parser, TokenType type) {
    return parser->current->type == type;
}

/* Consume current token if it's of a given type. Returns true if a token was consumed and false otherwise. */
static bool match(ASTParser* parser, TokenType type) {
    if (check(parser, type)) {
        advance(parser);
        return true;
    }
    return false;
}

// ---------------------------------------------------------------------------
// ------------------------------- PRODUCTIONS -------------------------------
// ---------------------------------------------------------------------------

/**
 * Terminal rule. Match identifier token.
 *
 * Returns true if this production was used, false otherwise.
 */
static Literal* identifierLiteral(ASTParser* parser) {
    Token* identifier = consume(parser, TOKEN_IDENTIFIER, "Expected identifier.");
    IdentifierLiteral* tempIdentifierLiteral = allocateASTNode(IdentifierLiteral);
    tempIdentifierLiteral->token = *(identifier);

    Literal* literal = allocateASTNode(Literal);
    literal->type = IDENTIFIER_LITERAL;
    literal->as.identifierLiteral = tempIdentifierLiteral;

    return literal;
}

/**
 * Terminal rule. Match number literal.
 *
 * Returns a pointer to a dynamically-allocated Literal.
 */
static Literal* numberLiteral(ASTParser* parser) {
    Token* currToken = consume(parser, TOKEN_NUMBER, "Expected number literal.");

    NumberLiteral* numberLiteral = allocateASTNode(NumberLiteral);
    numberLiteral->token = *(currToken);

    Literal* literal = allocateASTNode(Literal);
    literal->type = NUMBER_LITERAL;
    literal->as.numberLiteral = numberLiteral;

    return literal;
}

/**
 * primary-expression:
 * number-literal
 * string-literal
 * identifier
 * ( expression )
 * "true"
 * "false"
 * "null"
 * "this"
 *
 * Returns a pointer to a dynamically-allocated PrimaryExpression.
 */
static Expression* primaryExpression(ASTParser* parser) {
    switch (peek(parser)->type) {
        case TOKEN_NUMBER: {
            Literal* tempLiteral = numberLiteral(parser);

            Expression* expression = allocateASTNode(Expression);
            expression->type = PRIMARY_EXPRESSION;

            PrimaryExpression* primaryExpression = allocateASTNode(PrimaryExpression);
            primaryExpression->literal = tempLiteral;

            expression->as.primaryExpression = primaryExpression;

            return expression;
            break;
        }
        default: {
            errorAtCurrent(parser, "Expected a primary-expression.");
            return NULL;
            break;
        }
    }
}

/**
 * additive-expression:
 *  multiplicative-expression ( ( "-" | "+" ) multiplicative-expression )* ;
 *
 * Returns a pointer to a dynamically-allocated Expression.
 */
static Expression* additiveExpression(ASTParser* parser) {
    Expression* leftExpression = primaryExpression(parser);

    if (!check(parser, TOKEN_PLUS) && !check(parser, TOKEN_MINUS)) {
        errorAtCurrent(parser, "Expected '+' or '-' in an additive expression.");
    }
    Token* punctuator = peek(parser);
    advance(parser);

    Expression* rightExpression = primaryExpression(parser);

    AdditiveExpression* additiveExpression = allocateASTNode(AdditiveExpression);
    additiveExpression->leftExpression = leftExpression;
    additiveExpression->rightExpression = rightExpression;
    additiveExpression->punctuator = punctuator;

    Expression* expression = allocateASTNode(Expression);
    expression->type = ADDITIVE_EXPRESSION;
    expression->as.additiveExpression = additiveExpression;

    return expression;
}

/**
 * expression:
 *  struct-expression
 *  function-expression
 *  block-expression
 *  logical-or-expression
 *
 * Returns true if this production was used, false otherwise.
 */
static Expression* expression(ASTParser* parser) {
    switch (peek(parser)->type) {
        case TOKEN_NUMBER: {
            Expression* tempAdditiveExpression = additiveExpression(parser);
            return tempAdditiveExpression;
            break;
        }
        default:
            return NULL;
            break;
    }
}

/**
 * val-declaration:
 *  "val" identifier ( "=" expression )? ";"
 */
static Statement* valDeclaration(ASTParser* parser) {
    consume(parser, TOKEN_VAL, "Expected 'val' in a val declaration.");
    Literal* tempIdentifier = identifierLiteral(parser);
    consume(parser, TOKEN_EQUAL, "Expected '=' in a val declaration.");
    Expression* tempExpression = expression(parser);
    consume(parser, TOKEN_SEMICOLON, "Expected semicolon");

    ValDeclarationStatement* valDeclarationStatement = allocateASTNode(ValDeclarationStatement);
    valDeclarationStatement->identifier = tempIdentifier->as.identifierLiteral;
    valDeclarationStatement->expression = tempExpression;

    // Wrap ValDeclarationStatement in a Statement
    Statement* statement = allocateASTNode(Statement);
    statement->type = VAL_DECLARATION_STATEMENT;
    statement->as.valDeclarationStatement = valDeclarationStatement;
    return statement;
}

/**
 * declaration:
 *  var-declaration ";"
 *  val-declaration ";"
 *
 * Returns true if this production was used, false otherwise.
 */
static Statement* declaration(ASTParser* parser) {
    if (peek(parser)->type == TOKEN_VAL) {
        return valDeclaration(parser);
        // TODO: add VAR handling
    } else {
        errorAtCurrent(parser, "Expected 'var' or 'val' in a declaration.");  // Unreachable
        return NULL;
    }
}

/**
 * statement:
 *  declaration
 *  block-statement
 *  iteration-statement
 *  selection-statement
 *  return-statement
 *  expression-statement
 *  assignment-statement
 */
static Statement* statement(ASTParser* parser) {
    // NOTE
    // I don't love this, but in this `statement` function, but to make the parser efficient,
    // we need logic from the child productions. We need look ahead to see if the next token
    // matches the first token of a child production. If it does, we need to call that child.
    // This could be refactored so the logic to "check" is in the child functions; it'd be
    // cleaner IMO but probably less efficient.
    TokenType currentType = peek(parser)->type;
    switch (currentType) {
        // case TOKEN_VAR:
        case TOKEN_VAL:
            return declaration(parser);
        // case TOKEN_LEFT_CURLY:
        //     return blockStatement(parser);
        // case TOKEN_WHILE:
        //     return iterationStatement(parser);
        // case TOKEN_IF:
        //     return selectionStatement(parser);
        // case TOKEN_RETURN:
        //     return returnStatement(parser);
        default:
            errorAtCurrent(parser, "Error parsing statement. Expected a non-null statement.");
            return NULL;
            // return expressionStatement(parser);
    }
}

/**
 * Entry point for a program.
 *
 * source:
 *  statement* EOF
 */
static void source(ASTParser* parser) {
    while (!check(parser, TOKEN_EOF)) {
        if (parser->source->numberOfStatements >= MAX_NUMBER_STATEMENTS) {
            errorAtCurrent(parser, "Exceeded maximum number of statements.");
        }

        Statement* statementNode = statement(parser);

        if (statementNode != NULL) {
            parser->source->rootStatements[parser->source->numberOfStatements++] = statementNode;
        } else {
            exit(EXIT_FAILURE);
        }
    }
}

Source* parseAST(ASTParser* parser) {
    source(parser);
    return parser->source;
}

Source* parseASTFromTokens(ASTParser* parser, TokenArray* tokenArray) {
    initASTParser(parser, *tokenArray);
    Source* source = parseAST(parser);
    return source;
}