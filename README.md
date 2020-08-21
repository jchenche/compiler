# Compiler for Cool (Classroom Object Oriented Language)

The compiler was developed inside a linux virtual machine and compiled with g++ 9.3.0 which implements modern C++.

## Lexical Analysis
#### Flex was used to generate a lexer based on the lexical spec I provided. The spec involves a series of regular expressions with associated actions. As the lexer scans the source code, it tries to match the regex that has the largest number matching characters, and if there's a tie, it chooses the one that appears first. For rules that has start conditions, they'll only be matched if the lexer is in one of those states. The core file is in [./lexer/cool.flex](./lexer/cool.flex).
- Input: stream of characters.
- Function: groups characters into different types of tokens. Some tokens may have a value associated with it such as 12 for integer tokens.
- Output: stream of tokens.
###### Note: it uses flex-old for compability with the ancient support code.


## Syntactic Analysis
#### Bison was used to generate an LALR parser based on the formal grammar I provided. The spec involves a series of production rules with associated actions. The terminals are the different types of tokens such as integer and keyword. The parser resolves shift/reduce conflicts by choosing shift. The core file is in [./parser/cool.y](./parser/cool.y).
- Input: stream of tokens.
- Function: builds a tree that reflects the program's syntactic structure. Every node represents a syntactic construct and contains information about it. For example, nodes for **if-then-else** expressions contain fields for the sub-expressions of the construct.
- Output: abstract syntax tree.

## Semantic Analysis


## Code Generation
