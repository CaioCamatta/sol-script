# SolScript

SolScript is an interpreted programming language. Its syntax draws from Scala and C, while its internal stack-based virtual machine is inspired by Python and the JVM.

## Table of Contents

- [Features](#features)
- [Getting Started](#getting-started)
  - [Prerequisites](#prerequisites)
  - [Installation](#installation)
- [Usage](#usage)
- [Language Overview](#language-overview)
  - [Basic Syntax](#basic-syntax)
  - [Key Concepts](#key-concepts)
- [Language Design and Implementation](#language-design-and-implementation)
- [Development Status](#development-status)
- [Development](#development)

## Features

- **Virtual Machine**: Stack-based VM with fast local variable access and constant pool for string/number operands
- **First-Class Functions**: with lambda expressions
- **Expression-based design**: almost everything is an expression and yields a value
- **Object System**: basic struct-based object system with field access and mutation
- **Interactive Shell**: Built-in REPL
- **Debugging tools**: Robust debugging module for all components (scanner/parser/compiler/VM)

## Getting Started

Note: SolScript is a work-in-progress and has only been tested on ARM-based MacOS systems.

### Prerequisites

- C compiler (GCC)
- Make build system
- Git

### Installation

```bash
# Clone the repository
git clone https://github.com/CaioCamatta/sol-script.git
cd solscript

# Build the project
make

# For debug build
make debug
```

## Usage

Start the REPL:

```bash
./sol
```

Run a SolScript file:

```bash
./sol program.sol
```

## Language Overview

### Basic Syntax

Here's an example demonstrating a basic program:

```solscript
// Constants are immutable
val PI = 3.14159;

// Create an object with methods
var circle = struct {
  radius: 10;

  area: lambda () {
    PI * this.radius * this.radius
  };

  scale: lambda (var factor) {
    this.radius = this.radius * factor;
    this.area()
  };
};

// Method invocation
print circle.area();    // Output: 314.159
print circle.scale(2);  // Output: 1256.636
```

## Language Design and Implementation

SolScript has four main components:

- Scanner: reads user code and outputs tokens.
- Parser: consumes tokens and produces an abstract syntax tree (AST).
- Compiler: traverses the AST and produces bytecode.
- Virtual machine (VM): executes bytecode and maintains a stack of `Value`s.

![Architecture](./architecture.png)

The following sections contain details on each of the components.

<details>
<summary><strong>Lexical Grammar</strong></summary>

The scanner turns characters into tokens. For example, "val" becomes a `TOKEN_VAL`. It's inspired by the [C's lexical grammar](https://learn.microsoft.com/en-us/cpp/c-language/lexical-grammar?view=msvc-170) and [Scala's lexical expressions](https://www.scala-lang.org/files/archive/spec/2.11/06-expressions.html#expressions).

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

#### Example

When scanning `print 2+3-4;`, the following tokens would be produced:

```
TOKEN_PRINT(lexeme="print", line=1, column=6)
TOKEN_NUMBER(lexeme="2", line=1, column=8)
TOKEN_PLUS(lexeme="+", line=1, column=9)
TOKEN_NUMBER(lexeme="3", line=1, column=10)
TOKEN_MINUS(lexeme="-", line=1, column=11)
TOKEN_NUMBER(lexeme="4", line=1, column=12)
TOKEN_SEMICOLON(lexeme=";", line=1, column=13)
TOKEN_EOF(lexeme="", line=2, column=2)
```

Use `make debug` to view these tokens.

</details>

<details>
<summary><strong>Syntactical Grammar</strong></summary>

The syntactical grammar _should_ be an LALR(1) grammar. It can be parsed by a left-to-right parser with 1 tokens of look-ahead. A SolScript program `Source` is a series of `Statement`s that use `Expression`s and `Literal`s.

This grammar is inspired by the [ANSI C grammar](https://slebok.github.io/zoo/c/c90/sdf/extracted/index.html#Statement), [Lox](https://craftinginterpreters.com/) and Scala.

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
  expression "=" expression

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


lambda-expression:
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
  comparison-expression ( ("!=" | "==") comparison-expression )*

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
  primary-expression
  postfix-expression "(" ")"
  postfix-expression "(" argument-list ")"
  postfix-expression "." identifier

primary-expression:
  number-literal
  string-literal
  identifier
  block-expression
  ( expression )
  "true"
  "false"
  "null"
  "this"


number-literal      # terminal
string-literal      # terminal
identifier          # terminal
```

#### Example

When parsing `print 2+3-4;`, the following Abstract Syntax Tree would be produced:

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

</details>

<details>
<summary><strong>Compiled Code</strong></summary>

SolScript's code is compiled ahead of time. The compiled code object consists of:

- An array of bytecode with optional operands
- A constant pool for storing strings, numbers, and other constants
  - Necessary for communication .

Inspired by [java .class files](https://en.wikipedia.org/wiki/Java_class_file)

#### Example

When compiling `print 2+3-4;`, the following constants and bytecode would be produced:

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

</details>

<details>
<summary><strong>Virtual Machine (VM)</strong></summary>

The SolScript VM uses a stack-based architecture for executing bytecode instructions. Sol maintains a stack of `Value`s which can be numbers, strings, objects, etc. The language as a whole is optimized for fast code execution.

See [bytecode.h](./src/bytecode.h) for the complete Instruction Set.

Sol's VM is inspired by the [Lox VM](https://craftinginterpreters.com/a-virtual-machine.html) and the [JVM](https://docs.oracle.com/javase/specs/jvms/se8/html/), and to some extent, the [CPython VM](https://leanpub.com/insidethepythonvirtualmachine/read).

</details>

## Development Status

SolScript is currently in beta. It's feature-complete enough to play with.

### Completed Work

- ✅ Lexical grammar design
- ✅ sSntax grammar design
- ✅ Build configuration using Make
- ✅ Unit testing framework ([MinUnit](https://jera.com/techinfo/jtns/jtn002))
- ✅ REPL / interactive shell
- ✅ Hash table utility
- ✅ Array utility
- ✅ Robust debugging module for all parts of the system
- ✅ Completed scanner for the lexical grammar
- ✅ Implement minimal parser, compiler, and VM for end-to-end test.
- ✅ Constant pool (similar to [Java's](https://blogs.oracle.com/javamagazine/post/java-class-file-constant-pool))
- ✅ Variable declaration and access
- ✅ Print statements
- ✅ Additive expression and other "simple" expressions
- ✅ String literals
- ✅ Block statements
- ✅ [\_FAST](https://stackoverflow.com/questions/74998947/whats-pythons-load-fast-bytecode-instruction-fast-at) local variables similar to Python's
- ✅ Selection statement (`if`s)
- ✅ Conditional debugging/logging for tests that fail
- ✅ Block expressions
- ✅ Assignment statements
- ✅ Non-constant variables
- ✅ Iteration statement (loops)
- ✅ Functions and returns
- ✅ End-to-end tests
- ✅ Separate build with debug logs
- ✅ Panic Mode error recovery in the Parser (prevent crashing on every error).
- ✅ Objects / structs
- ✅ Recursion support

### Roadmap to v1.0

Features

- [ ] Support `this` reference in objects
- [ ] Add garbage collector

Small items and fixes:

- [x] Fix chained function calls
- [ ] Audit all dynamic memory allocation
- [ ] If-expressions at the end of function blocks should be used as function return.
- [ ] Returning null from functions doesnt work in structs
- [ ] Explicitly fail when closing over variables

### Future Work (v1.1+)

These will be great to have but left out of scope for v1:

- [ ] Native functions
- [ ] Prototypal inheritance
- [ ] Benchmarking utility
- [ ] Performance profiling and optimization
- [ ] Panic Mode error recovery for the Compiler
- [ ] Better compiler error logging with line and column printing
- [ ] [NaN boxing](https://piotrduperas.com/posts/nan-boxing) for smaller bytecode
- [ ] Array data structure
- [ ] Closures

## Development

### Philosophy

SolScript should be simple and well-documented. Runtime performance should be favoured over compilation time; inefficiencies in the Scanner, Parser, or Compiler are acceptable.

### Project structure

```python
src/ # .c and .h source files
src/util/ # .c and .h utility files
test/unit/src/ # unit tests for the main modules
test/unit/src/util/ # unit tests for the utilities
test/end_to_end/ # end-to-end tests written in SolScript
test/manual/ # SolScript code/scenarios that can be ran manually
```

### Testing

```bash
# Run all tests
make test
```
