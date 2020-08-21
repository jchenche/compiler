# Compiler for Cool
Cool is an object-oriented language with static typing. This compiler was developed inside a Linux virtual machine and compiled with g++ 9.3.0 which implements modern C++.

## Lexical Analysis
#### The core file is [lexer/cool.flex](lexer/cool.flex).
- Input: stream of characters.
- Purpose: groups characters into different types of tokens. Some tokens may have a value associated with it such as 12 for integer tokens.
- Output: stream of tokens.
- Description:
  - I used Flex to generate a lexer based on the lexical spec I provided. The spec involves a series of regular expressions with associated actions.
  - As the lexer scans the source code, it tries to match the regex that has the largest number matching characters, and if there's a tie, it chooses the one that appears first.
  - For rules that has start conditions, they'll only be matched if the lexer is in one of those states.

###### Note: it uses flex-old to resolve compability issues with the legacy support code.

## Syntactic Analysis
#### The core file is [parser/cool.y](parser/cool.y).
- Input: stream of tokens.
- Purpose: builds a tree that reflects the program's syntactic structure. Every node represents a syntactic construct and contains information about it. For example, nodes for **if-then-else** expressions contain fields for the sub-expressions of the construct.
- Output: abstract syntax tree.
- Description:
  - I used Bison to generate an LALR parser based on the formal grammar I provided. The spec involves a series of production rules with associated actions.
  - The terminals are the different types of tokens such as integer and keyword.
  - Precedence declarations are used to resolve ambiguities such as shift/reduce conflicts; otherwise, the parser will resolve it by choosing to shift.

## Semantic Analysis
#### The core file is [semant/semant.cc](semant/semant.cc).
- Input: abstract syntax tree.
- Purpose: type-checks and assigns a static type to each expression under an environment which consists of a list of scopes where the scopes are lists of identifier and type pairs. It also makes sure that parameter names are unique, reference to names are defined in scope, and that the class hierarchy is acyclic, etc.
- Output: typed annotated abstract syntax tree.
- Description:
  - The analyzer builds a class hierarchy and stores it in a hash table that maps class names to their hierarchy nodes.
  - After that, it gathers and stores all method signatures in a hash table that maps class names to another hash table that maps method names (excluding inherited ones) to their signatures.
  - It also gathers and stores all attribute names in a hash table that maps class names to another hash table that maps attribute names (excluding inherited ones) to their types.
  - Then it begins type-checking by traversing the tree.

## Code Generation
#### The core file is [cgen/cgen.cc](cgen/cgen.cc).

