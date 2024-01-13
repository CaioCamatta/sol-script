# Delta-lang

Delta is a stack-based, prototype-based, interpreted, GC-ed programming language.

Developed in C17.

## Language Design

![Architecture](./architecture.png)

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

```
urce: 
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
	relational-expression ( ("!=" | "==") relational-expression) )*

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
