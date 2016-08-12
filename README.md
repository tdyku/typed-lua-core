# Typed Lua Core Typechecker
[![Build Status](https://travis-ci.org/tomkee/typed-lua-core.svg?branch=master)](https://travis-ci.org/tomkee/typed-lua-core)

The idea of project is to use Haskell as another way of reasoning about the typing rules of Typed Lua. Project involves implementing parser and typechecker for Typed Lua Core.

**[Typed Lua](https://github.com/andremm/typedlua)** is a typed superset of Lua that compiles to plain Lua. It provides optional type annotations, compile-time type checking, and class-based object oriented programming through the definition of classes, interfaces, and modules.

**Typed Lua Core** is reduced version of Typed Lua prepared to present rules of Typed Lua type system. 

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

For more informations about TypedLua typesystem and Typed Lua Core syntax please read [Typed Lua: An Optional Type System for Lua](https://github.com/andremm/typedlua/blob/master/doc/thesis/thesis_andre_certified.pdf).


## Overview of project
Compilation can be divided into 3 phases:
1. Parsing
2. Resolving global variables
3. Typechecking

### 0. AST and type hierarchy
Typed Lua Core AST is quite similar to [Typed Lua AST](https://github.com/andremm/typedlua/blob/master/typedlua/tlast.lua). It can be found in file `src/AST.hs`.

Type hierarchy is implemented in file `src/Types.hs`.


### 1. Parsing
Parsing is done with haskell [trifecta](https://hackage.haskell.org/package/trifecta) library.

Source code of type parser can be found in `src/Parser/Types.hs`. 

Typed lua core expressions and statements parser is implemented in file `src/Parser/Code.hs`.

### 2. Resolving global variables
Typed lua core supports only local variable declarations. All global variables are in fact members of unique table `_ENV`. Because of this all reads and writes of global variables should be translated to table access.
In example:
`b = 1`
should be translated to:
`_ENV["b"] = 1`.

Moreover global table `_ENV` should be declared explicitly in top, local scope:
```
local _ENV:{"a":integer, "b":integer} = {["a"] = 1, ["b"] = 1}
in b = a * b
```

Because of this I implemented compiler pass called **Globals transformation** which was detecting global variables and transforming them to global table access.
Source code of **global transformation** can be found in `src/Transform/Globals`.

### 3. Typechecking
Typed Lua Core Typechecker consists of few kinds of rules:
* Subtyping rules
* Typing rules:
    * Statements typing rules
    * Expressions typing rules

#### Subtyping rules
Subtyping rules are implemented in file `src/Typechecker/Subtype.hs`.
They can be easily extended. In order to to this you just need to edit or add new implementation of some pattern in method `<?`.

During GSOC 2016 I implemented subtyping rules for:
* literal types
    * literal string
    * literal float
    * literal booleans
    * literal integer
* basic types
    * integer
    * string
    * number
    * bool
* nil values
* dynamic any
* `value` type
* self type
* unions
* functions
* tuples
* varargs
* tuple unions
* tables
* table fields
* recursion
    * amber rule
    * assumption
    * left-right unfolding
* expression types
    * projection types
    * filters
    * tuples of expression types

#### Typing rules
Both statements and expressions typing rules are implemented in file `src/Typechecker/Type.hs`
##### Statements typing rules
Rules can be edited/extended by adding entries to function `tStmt`.
Statement typing rules implemented during GSOC 2016:
* skip
* assignment
* typed/untyped local declaration
* recursive declaration
* method declaration
* while & if
* return statement
* void function and method call

##### Expressions typing rules
Rules can be edited/extended by adding entries to function `getTypeExp`.

Expression typechecking rules I implemented:
* literals
* variable type reading
* table index reading
* type coercions
* function definitions
* table constructors
* fields declaration
* binary operators:
    * relational
    * arithmetical
* unary operators
* function and method calls
* variable writing
* index writing
* table refinements
* expression list
* varargs
* self
* nil
* metatables
* recursion

## Examples
Directory `examples/` contains Typed Lua Core source code which is used by test suite to perform typechecking. Examples are structured in several categories:
* Simple examples
* Statements
* Tables
* Object oriented programming
* Recursion
* Metatables

Examples overview:
### Simple examples
* `examples/simple/source1.tlc`
```
local res:number, a:integer, b:number = 0, 1, 0.5 in
    res = a + b
```
Basic typed local declaration, show binary operations and that adding `number` to `integer` gives `number`.
* `examples/simple/source2.tlc`
```
local len:integer, word:string = 0, "testingWord" in
    len = #word
```
Example of unary operation - hash - which returns `integer`.
* `examples/simple/source3.tlc`
```
local _ENV:{"b":integer}_unique = {["b"]=0} in  
    local a = 2 in 
        b = a + 2
```
Example of using global variables - firstly we need to declare them explicitly in outer table - `_ENV`, then compiler resolves `b` to `_ENV["b"]`.

### Statements
* `examples/statements/source1.tlc`
```
local error,result = "",1 in
local idiv = fun(dividend:integer, divisor:integer): ((integer, integer)|(nil, string))
        local q,r = 0,0 in
            if divisor == 0 then
                return nil, "divZero"
            else
                r = dividend / divisor;
                q = dividend % r;
                return q, r 
in
    local p,q = idiv(10,2) in
        if p then result = q else error =  q
                
```
Example which briefly depicts work of projection types. `idiv` has type `integer,integer` or `nil,string`. Inside `if` statement we can project union of these tuples for proper type. Inside `then` variable `q` has type `integer`, inside `else` - `string`.
### Tables
* `examples/tables/source1.tlc`
```
local idiv = fun(dividend:integer, divisor:integer): ((integer, integer)|(nil, string))
    skip 
in
    local a = {["x"] = 1, ["y"] = 2, idiv(10, 5)} in
        skip
```
Basic table construction - we constructed table of type `{"x":integer, "y":integer, 1:(integer|nil), 2:(integer|string)}`.
* `examples/tables/source2.tlc`
```
local person = {} in
    person["firstname"] <string> = "Lou";
    person["lastname"] <string> = "Reed"
```
Basic table refinement - we construct unique table `{}` but then extend it with new fields.
* `examples/tables/source3.tlc`
```
local a : {}_unique = {} in
local b : {}_open = a in
    a["x"] <string> = "foo";
    b["x"] <integer> = 1
```
Example of code which won't typecheck as aliasing a produces the type `{}_closed` that is not a subtype of `{}_open`. 
* `examples/tables/source4.tlc`
```
local a : {}_unique = {} in
    a["x"] <string> = "foo";
    a["y"] <string> = "bar";
    local b : {"x" : string, "y" : (string|nil)}_closed = <{"x" : string, "y" : (string|nil)}_open> a in 
        a["z"] <integer> = 1
```
Type coercion in work - coercion converts the type of a from `{"x" : string, "y" : string}_unique` to `{"x" : string, "y" : string|nil}_open`,
and results in `{"x" : string, "y" : string|nil}_closed`, which is a subtype of
`{"x" : string, "y" : string|nil}_closed`, the type of b.
* `examples/tables/source5.tlc`
```
local b:{string:(integer|nil), "z":(1|nil)}_closed = {["x"]=1, ["y"]=2} in
   skip   
```
This example typechecks because of subtyping rules between `closed` and `unique` tables.
### Object oriented programming
* `examples/objects/source1.tlc`
```
local tab:{number:string}_unique = {[1] = "jeden"} in
    fun tab:a(age:number):(number) return age
```
Basic method example - we are making new - unique - table, then we are adding new method to it.
* `examples/objects/source2.tlc`
```
local tab:{"a":(number) -> (number)}_unique 
        = { ["a"] = fun(x:number):(number) return x } in
    fun tab:a(age:number):(number) return age
```
Here we are also adding method to table, but this time similar method already existed in table. It is possible because type of method is subtype of existing method.
### Recursion
* `examples/recursion/source1.tlc`
```
rec a : ux.{"next":(x|nil)} = {["next"]={["next"] = nil  }} in
    skip
```
Example of unfolding recursion rule - we are using it to replace `x` with `ux.F` itself.
### Metatables
* `examples/recursion/source1.tlc`
```
local tab = {} in
    fun tab:foo():(number) |setmetatable({}, {["index"] = tab})|; return 0
```
Basic metatable example - we are setting metatable for this method to already set one - `tab`.
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