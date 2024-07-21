# SolScript

SolScript is an interpreted, stack-based, prototype-based, garbage-collected programming language.

To get started, clone the repository, run `make` to build the project or `make debug` to include debug logs. Run `./sol` to start the REPL or `./sol program.sol` execute a SolScript program.


**Important:** SolScript is a [work in progress](https://github.com/CaioCamatta/sol-script?tab=readme-ov-file#v10-release-tracker).


## Example program
```
// Constant declaration declaration
val a = 1;

// Example object
var uniqueNumberGenerator = struct {
  currNumber: 0;

  // Print N new unique numbers and return the last unique one.
  printNewNumbers: lambda (var count) {
    while (count > 0){
      print count;
      this.currNumber = this.currNumber +1;
      count = count - 1;
    }
    this.currNumber; // In a block expression, no 'return' keyword is necessary
  };
}

// Prints '0' and '1'.
uniqueNumberGenerator.printNewNumbers(2);
```

## Language Design

The following diagram gives a high-level overview of SolScript's internals.

SolScript has four main components: scanner, parser, compiler, and virtual machine. The scanner reads user code and outputs tokens. The parser consumes tokens and produces and abstract syntax tree (AST). The compiler traverses the AST and produces bytecode. The VM executes bytecode and keeps a stack of Values.

![Architecture](./architecture.png)

### Lexical Grammar

The scanner is a regular language that turns characters into tokens. For example, "val" becomes a `TOKEN_VAL`.

The following is SolScript's lexical grammar. It's inspired by the [C lexical grammar](https://learn.microsoft.com/en-us/cpp/c-language/lexical-grammar?view=msvc-170), and [Scala lexical expressions](https://www.scala-lang.org/files/archive/spec/2.11/06-expressions.html#blocks).

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

Note that comments are treated as whitespace.

#### Example
For the statement `print 2+3-4;`, the scanner would parse the following tokens:
```
TOKEN_PRINT(lexeme="print", line=1, column=6)
TOKEN_NUMBER(lexeme="2", line=1, column=8)
TOKEN_PLUS(lexeme="+", line=1, column=9)
TOKEN_NUMBER(lexeme="3", line=1, column=10)
TOKEN_MINUS(lexeme="-", line=1, column=11)
TOKEN_NUMBER(lexeme="4", line=1, column=12)
TOKEN_SEMICOLON(lexeme=";", line=1, column=13)
TOKEN_EOF(lexeme="", line=2, column=2)
``````

### Syntactical Grammar

The syntactical grammar _should_ be an LALR(2) grammar, i.e. it can be parsed by a left-to-right parser with 2 tokens of look-ahead. In SolScript, a program Source is a series of Statements. Statements use Expressions and Literals. Expressions are evaluated to a Value at run time.

This grammar is primarily inspired by the [ANSI C grammar](https://slebok.github.io/zoo/c/c90/sdf/extracted/index.html#Statement), [Lox](https://craftinginterpreters.com/) and Scala.

```
source: 
  statement* EOF

statement: 
  declaration
  block-statement
  iteration-statement
  selection-statement
  return-statement
  print-statement
  assignment-statement
  expression-statement # if this is a call expresison or an identifier, check next character. If next is  a "." 

declaration:
  var-declaration
  val-declaration

var-declaration:
  "var" identifier ";"
  "var" identifier "=" expression  ";"

val-declaration:
  "val" identifier "=" expression ";"

block-statement:
  "{" statement* "}"

iteration-statement:
  "while" "(" expression ")" block-statement

selection-statement:
  "if" "(" expression ")" statement 
  "if" "(" expression ")" statement "else" statement

return-statement:
  "return" ";"
  "return" expression ";"
  
expression-statement:
  expression ";"

assignment-statement: 
  expression "=" expression  # the compiler ensures the expression is a valid target of assigment.

print-statement:
  "print" expression ";"


expression:
  struct-expression
  function-expression
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
  "lambda" "(" ")" "{" block-expression "}"
  "lambda" "(" parameter-list ")" "{" block-expression "}"
  
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
  postfix-expression
  ( "!" )* postfix-expression
  ( "-" )* postfix-expression

postfix-expression:
  postfix-call-expression
  postfix-call-expression "." postfix-expression
  "this" "." postfix-expression
  identifier "." postfix-expression

postfix-call-expression:
  primary-expression
  identifier "(" ")"  
  identifier "(" argument-list ")"  

primary-expression:
  number-literal
  string-literal
  identifier
  block-expression
  ( expression )
  "true"
  "false"
  "null"


number-literal      # terminal
string-literal      # terminal
identifier          # terminal
```

#### Example
For the statement `print 2+3-4;`, the scanner would parse the following Abstract Syntax Tree:

```
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
```

### Compiled code

SolScript's compiled code is heavily inspired by [java .class files](https://en.wikipedia.org/wiki/Java_class_file). SolScript. It has two parts:
- An array of bytecode. Bytecode have optional operands.
- A pool of constants

The constant pool is necessary so the compiler can pass constants, such as strings for variable names, to the VM.

#### Example
For the statement `print 2+3-4;`, the compiler would produce the following constants and bytecode:
```
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
```

### The Virtual Machine

The SolScript Virtual Machine* is heavily inspired by the [Lox VM](https://craftinginterpreters.com/a-virtual-machine.html) and the [JVM](https://docs.oracle.com/javase/specs/jvms/se8/html/), and to some extent, the [CPython VM](https://leanpub.com/insidethepythonvirtualmachine/read). 

*For those new to programming languages, a "virtual machine" here is not the same as a "Windows virtual machine". A programming language VM translates bytecode down to low-level code or machine code. 

See [bytecode.h](./src/bytecode.h) for the complete Instruction Set.

A runtime `Value` could be a number, string, object, etc.

## Development

### Philosophy

SolScript is a "toy" programming language. Its main advantage over "real" languages, if any, its simplicity and the ease with which one can learn its internals. Learning this codebase should be exceptionally easy. Hence, comments and documentation should be extensive.

Additionally, run time performance is more critical than compilation time performance. Inefficiencies in the scanner, parser, and compiler are acceptable.

### Project structure

`src/` - `.c` and `.h` source files

`src/util/` - `.c` and `.h` utility files

`test/unit/src/` - unit tests for the C code

`test/unit/src/util/` - unit tests for the utilities C code

`test/sol/` - tests written in SolScript


## v1.0 Release Tracker

The following features are necessary a proper v1.0 release, in rough order:

 - [x] Design the lexical grammar
 - [x] Design the syntax grammar
 - [x] Add build configuration using make
 - [x] Add unit testing framework ([MinUnit](https://jera.com/techinfo/jtns/jtn002))
 - [x] Add REPL / interactive shell
 - [x] Implement and test hash table utility
 - [x] Implement and test array utility
 - [X] Implement robust debugging module for all parts of the system.
 - [x] Implement full scanner for the lexical grammar
 - [x] Implement minimal parser, compiler, and VM for end-to-end test.
 - [x] Add constant pool (similar to [Java's](https://blogs.oracle.com/javamagazine/post/java-class-file-constant-pool))
 - [X] Add support for variable declaration and access
 - [X] Implement print statements
 - [X] Implement additive expression
 - [X] Implement all other "simple" expressions, i.e. excluding postfix-call-expressions
 - [X] Implement string literals
 - [X] Implement block statements
 - [X] Add [_FAST](https://stackoverflow.com/questions/74998947/whats-pythons-load-fast-bytecode-instruction-fast-at) local variables
 - [X] Implement selection statement (`if`s)
 - [X] Add conditional debugging/logging for tests that fail
 - [X] Implement block expressions
 - [X] Implement assignment statements
 - [X] Implement non-constant variables
 - [X] Implement iteration statement (loops)
 - [X] Implement functions and returns
 - [X] Add end-to-end tests
 - [x] Make debug logs optional for REPL or program execution
 - [X] Add Panic Mode error recovery to parser; stop crashing on every error.
 - [ ] Implement objects / structs
 - [ ] Implement closures
 - [ ] Add garbage collector

## v1.1 Tasks
 - [ ] Add native functions
 - [ ] Add benchmark tests
 - [ ] Profile execution and find opportunities for optimization
 - [ ] Add Panic Mode error recovery to compiler
 - [ ] Improve compiler error logging; print line and column
 - [ ] Implement [NaN boxing](https://piotrduperas.com/posts/nan-boxing)
