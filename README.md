# Compiler for Cool
Cool is an object-oriented language with static typing. This compiler was developed inside a Linux virtual machine and compiled with g++ 9.3.0 which supports modern C++.

## Lexical Analysis
#### The core file is [lexer/cool.flex](lexer/cool.flex)
- Input: Cool source code, a stream of characters.
- Purpose: groups characters into different types of tokens. Some tokens may have a value associated with them such as 12 for integer tokens.
- Output: stream of tokens.
- Description:
  - I used Flex to generate a lexer based on the lexical spec I provided. The spec involves a series of regular expression rules with associated actions.
  - As the lexer scans the source code, it tries to pick the regex that has the largest number of matching characters, and if there's a tie, it picks the one that appears first.
  - For rules that have start conditions, they'll only be active if the lexer is in one of those conditions.

###### Note: it uses flex-old to resolve compability issues with the legacy support code.

## Syntactic Analysis
#### The core file is [parser/cool.y](parser/cool.y)
- Input: stream of tokens.
- Purpose: builds a tree that reflects the program's syntactic structure. Every node represents a syntactic construct and contains information about it. For example, nodes for **if-then-else** expressions contain fields for the sub-expressions of the construct.
- Output: abstract syntax tree.
- Description:
  - I used Bison to generate an LALR parser based on the formal grammar I provided. The spec involves a series of production rules with associated actions.
  - The terminals are the different types of tokens such as integer and keyword.
  - Precedence declarations are used to resolve ambiguities such as shift/reduce conflicts; otherwise, the parser will resolve it by choosing to shift.

## Semantic Analysis
#### The core file is [semant/semant.cc](semant/semant.cc)
- Input: abstract syntax tree.
- Purpose: type-checks and assigns a static type to each expression under an environment which consists of a list of scopes where the scopes are lists of (name, type) pairs. It also makes sure that parameter names are unique, reference to names are defined in scope, and that the class hierarchy is acyclic, etc.
- Output: typed annotated abstract syntax tree.
- Description:
  - The analyzer builds a class hierarchy and stores it in a hash table that maps class names to their hierarchy nodes.
  - After that, it gathers and stores all method signatures in a hash table that maps class names to another hash table that maps method names, excluding inherited ones, to their signatures. It also gathers and stores all attribute names in a hash table that maps class names to another hash table that maps attribute names, excluding inherited ones, to their types.
  - Then it begins type-checking by traversing the tree.

## Code Generation
#### The core file is [cgen/cgen.cc](cgen/cgen.cc)
- Input: typed annotated abstract syntax tree.
- Purpose: For each expression node in the tree, it generates stack-preserving code that evaluates the expression, under an environment, to a value or object (all Cool values are objects), and stores it in `$a0`. It also generates code for constants, method definitions, prototype objects, dispatch tables, etc. Some labels are declared global in order to interface correctly with the provided runtime system. e.g. the main method from the Main class must be visible to the runtime system during execution.
- Output: MIPS assembly code, to be executed by a MIPS processor simulator called SPIM.
- Description:
  - The code generator builds a class hierarchy like in the previous phase, but in a different way, and with more information embedded in the hierarchy nodes.
  - It traverses the tree to determine, for each method (including object initializers), the maximum number of local variables being used at any point during evaluation, and reserves that much space in the stack for them. A normal method gets that number from its body, but for an object initializer, since it executes each attribute's initializer one after the other, it collects such number from each initializer and takes the max out of them.
  - An environment is used to determine the memory location of variable names during evaluation. It is a list of scopes where the scopes are lists of (name, locator) pairs. I designed the locator to be a (variable type, offset) pair because at runtime, the location of an attribute is found at an offset from `$s0`. For parameters and local variables, `$fp` is used instead, and for local variables alone, the offset is negated.
  - For the locating mechanism to work, attributes must be located in the same position for every class and its descendants. So it builds a hash table that maps class names to vectors of (attribute name, type) pairs, including inherited attributes. It also builds a similar table for methods, but with (method name, full method name) pairs, including inherited methods, to facilitate method lookups from dispatch tables.
