# Delta-lang

Delta is a stack-based, prototype-based, interpreted, GC-ed programming language.

Developed in C17.

# Language Design

## 1. Scanner

The scanner is a regular language that turns characters into tokens. For example, the "val" becomes "TOKEN_VAL".

The following is Delta's lexical grammar. It's insipired by [Lox's lexical grammar](https://craftinginterpreters.com/appendix-i.html), the [C lexical grammar](https://learn.microsoft.com/en-us/cpp/c-language/lexical-grammar?view=msvc-170) , and [Scala's lexical syntax](https://www.scala-lang.org/files/archive/spec/2.12/01-lexical-syntax.html#identifiers).

```
TOKEN           -> KEYWORD | IDENTIFIER | KEYWORD | STRING-LITERAL | PUNCTUATOR
KEYWORD         -> "number" | "if" | "else" | "struct" | "return" | "false" | "true" | "null" | "val"
NUMBER          -> DIGIT+ (. DIGIT+)?
STRING-LITERAL  -> "\" "[^\"]* "\""
IDENTIFIER      -> NONDIGIT (NONDIGIT | DIGIT)*
NONDIGIT        -> "a" - "z" | "A" - "Z" | "_"
DIGIT           -> "0" - "9"
PUNCTUATOR      -> "(" | ")" | "{" | "}" | "." | "*" | "+" | "-" | "!" | "%" | "<" | ">" | "=" | "<=" | ">=" | "==" | "!=" | "||" | "&&" | ";"
```
