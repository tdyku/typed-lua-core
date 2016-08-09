# Typed Lua Core Typechecker
[![Build Status](https://travis-ci.org/tomkee/typed-lua-core.svg?branch=master)](https://travis-ci.org/tomkee/typed-lua-core)

The idea of this project is to use Haskell as another way of reasoning about the typing rules of Typed Lua. Project involves implementing parser and typechecker for Typed Lua Core.

[Typed Lua](https://github.com/andremm/typedlua) is a typed superset of Lua that compiles to plain Lua. It provides optional type annotations, compile-time type checking, and class-based object oriented programming through the definition of classes, interfaces, and modules.

Typed Lua Core is reduced version of Typed Lua prepared to present rules of Typed Lua type system. 
Typed Lua Core contains:
* multiple assignments
* local (typed and untyped) declarations
* if and while statements
* binary operations: +, .., ==, <, /, %, // 
* unary operations: #, not
* explicit type annotations
* tables
* functions and function calls
* explicit method declarations and calls
* table refinements
* type coercions
* recursive types

Typed Lua core does not support features and syntactic sugar as:
* labels and goto
* repeat-until, for
* table fields other than [ x ] = y
* arithmetic operators other than +, /, %, //
* relational operators other than == and <
* bitwise operators other than &
* unary opertors other than # and not


## Overview of project
Compilation can be divided into 3 phases:
1. Parsing
2. Resolving global variables
3. Typechecking

### 1. Parsing
Parsing is done with haskell [trifecta](https://hackage.haskell.org/package/trifecta) library.

Source code of type parser can be found in `src/Parser/Types.hs`. 

Typed lua core expressions and statements parser is implemented in file `src/Parser/Code.hs`.

### 2. Resolving global variables
Typed lua core supports only local variable declarations. All global variables are in fact members of unique table `ENV`. Because of this all reads and writes of global variables should be translated to table access.
In example:
`b = 1`
should be translated to:
`ENV["b"] = 1`.

Moreover global table `ENV` should be declared explicitly in top, local scope:
```
local ENV:{"a":integer, "b":integer} = {["a"] = 1, ["b"] = 1}
in b = a * b
```

Because of this I implemented compiler pass called **Globals transformation** which was detecting global variables and transforming them to global table access.
Source code of **global transformation** can be found in `src/Transform/Globals`.

### 3. Typechecking
Typed Lua Core Typechecker consisted of


## Installation
* Download & install [stack](https://docs.haskellstack.org/en/stable/README/)
* `stack setup` to install proper version of ghc
* `stack init` to create stack project files
* `stack build` to compile project

Generated library can be found in`.stack-work/install/x86_64-linux/lts-5.4/7.10.3/bin/coreTL`.
Generated binary should be run with one arg - path to file you want to parse and typecheck.
  
## Running test suite
Tests are divided into following groups:
* Parser `test/Test/Parser.hs`
* Typechecker
	* Subtyping `test/Test/Typechecker/Subtyping.hs`
	* Typechecking `test/Test/Typechecker/Typechecker.hs`  

Test suite can be executed with command `stack test`.