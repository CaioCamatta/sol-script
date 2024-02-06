# Delta-lang

Delta is a stack-based, prototype-based, interpreted, GC-ed programming language developed in C.

## Language Design

![Architecture](./architecture.png)

### Example 

Delta has a debug module that shows the tokens parsed, the AST, and the compiled code. Here's an example for the statement `print 2+3-4;`.

```
% ./delta   
> print 2+3-4;
Tokens:
  TOKEN_PRINT(lexeme="print", line=1, column=6)
  TOKEN_NUMBER(lexeme="2", line=1, column=8)
  TOKEN_PLUS(lexeme="+", line=1, column=9)
  TOKEN_NUMBER(lexeme="3", line=1, column=10)
  TOKEN_MINUS(lexeme="-", line=1, column=11)
  TOKEN_NUMBER(lexeme="4", line=1, column=12)
  TOKEN_SEMICOLON(lexeme=";", line=1, column=13)
  TOKEN_EOF(lexeme="", line=2, column=2)

AST
Source(numberOfStatements=1)
|   PrintStatement
|   |   AdditiveExpression(punctuator="-")
|   |   |   (left)
|   |   |   |   AdditiveExpression(punctuator="+")
|   |   |   |   |   (left)
|   |   |   |   |   |   PrimaryExpression
|   |   |   |   |   |   |   NumberLiteral(token="2")
|   |   |   |   |   (right)
|   |   |   |   |   |   PrimaryExpression
|   |   |   |   |   |   |   NumberLiteral(token="3")
|   |   |   (right)
|   |   |   |   PrimaryExpression
|   |   |   |   |   NumberLiteral(token="4")

Started compiling.
Done compiling in 0.00001 seconds.

Compiled code:
Constant Pool 
 #0 (double) 2.000000
 #1 (double) 3.000000
 #2 (double) 4.000000
Bytecode
 [ LOAD_CONSTANT #0 ]
 [ LOAD_CONSTANT #1 ]
 [ ADD ]
 [ LOAD_CONSTANT #2 ]
 [ PRINT ]

4.000000
```

### 1. Scanner

The scanner is a regular language that turns characters into tokens. For example, the "val" becomes "TOKEN_VAL".

The following is Delta's lexical grammar. It's insipired by [Lox's lexical grammar](https://craftinginterpreters.com/appendix-i.html), the [C lexical grammar](https://learn.microsoft.com/en-us/cpp/c-language/lexical-grammar?view=msvc-170) , and [Scala's lexical syntax](https://www.scala-lang.org/files/archive/spec/2.12/01-lexical-syntax.html#identifiers).

```
token:
	keyword
	identifier
	number-literal
	string-literal
	punctuator

keyword:
	"number" 
    "if" 
    "else" 
    "struct" 
    "return" 
    "false" 
    "true" 
    "null"
    "val"
    "prototype"

number-literal:
	digit+ (. digit+)?

string-literal:
	" s-char-sequence* "

s-char-sequence:
	[^\"]

identifier: 
	non-digit (non-digit | digit)*

non-digit: one of
	 _ a b c d e f g h i j k l m
	 n o p q r s t u v w x y z
	 A B C D E F G H I J K L M
	 N O P Q R S T U V W X Y Z

digit: one of
	0 1 2 3 4 5 6 7 8 9

punctuator: one of
	( ) { } . * + - ! % < > = <= >= == != || && ; ,
```

### 2. Parser

The syntactical grammar implemented by the Parser _should_ be parsable a look-ahead, left-to-right grammar with 1 token of look-ahead (aka LALR(1)). A program (`source`) is a series of `statement`s. Statements have side effects (or they aren't useful). `expression`s and `literal`s are the other two main "types". Expressions are evaluated to a value.

This grammar is primarily inspired by the [ANSI C grammar](https://slebok.github.io/zoo/c/c90/sdf/extracted/index.html#Statement) and Scala.

```
source: 
	statement* EOF

statement: 
	declaration
	block-statement
	iteration-statement
	selection-statement
	return-statement
	expression-statement
	assignment-statement
	print-statement

declaration:
	var-declaration
	val-declaration

var-declaration:
	"var" identifier ( "=" expression )?  ";"

val-declaration:
	"val" identifier "=" expression ";"

block-statement:
	"{" statement* "}" ";"
	block-expression

iteration-statement:
	"while" "(" expression ")" block-statement

selection-statement:
	"if" "(" expression ")" statement ( "else" statement )?

return-statement:
	"return" ( expression )? ";"
	
expression-statement:
	expression ";"

assignment-statement:
	postfix-expression "=" expression

print-statement:
	"print" expression ";"


expression:
	struct-expression
	function-expression
	block-expression
	logical-or-expression



struct-expression:
	"struct" "{" struct-declaration-list "}"

struct-declaration-list:
	struct-declaration
	struct-declaration-list "," struct-declaration
	
struct-declaration:
	identifier ":" expression
	"prototype" ":" identifier



function-expression:
	"(" (parameter-list)? ")" "=>" "{" statement "}"
	
parameter-list:
	identifier ( "," identifier )*

argument-list:
	expression ( "," expression )*



block-expression:
	"{" statement* expression "}"



logical-or-expression:
	logical-and-expression ( "or" logical-and-expression )*

logical-and-expression:
	equality-expression ( "and" equality-expression )*

equality-expression: 
	comparison-expression ( ("!=" | "==") comparison-expression) )*

comparison-expression:
	additive-expression ( ( ">" | ">=" | "<" | "<=" ) additive-expression )*

additive-expression:
	multiplicative-expression ( ( "-" | "+" ) multiplicative-expression )* 
	
multiplicative-expression:
	unary-expression ( ( "/" | "*" ) unary-expression )* 

unary-expression:
	( "!"* | "-"* )? postfix-expression

postfix-expression:
	primary-expression
	postfix-expression "." identifier  

primary-expression:
	number-literal
	string-literal
	identifier
	( expression )
	"true"
	"false"
	"null"
	"this"

call-expression:
	unary-expression
	

number-literal      # terminal
string-literal      # terminal
identifier-literal  # terminal
```

## Development

### Project structure

`src/` - `.c` and `.h` source files

`src/util/` - `.c` and `.h` utility files

`test/unit/src/` - unit tests for the C code

`test/unit/src/util/` - unit tests for the utilities C code

`test/delta/` - tests for the Delta language
