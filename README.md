[![Build Status](https://travis-ci.org/tomkee/typed-lua-core.svg?branch=master)](https://travis-ci.org/tomkee/typed-lua-core)

# Installation
There are 2 ways of installing compiler on your machine:
  1. **cabal** If you have well-configured ghc, Cabal and cabal-install just type `cabal install` and run generated binary
  2. **stack**. `stack setup` to install proper version of ghc, `stack init` to initialize **stack** project and `stack build` to install project. Generated library could be find in `.stack-work/install/x86_64-linux/lts-5.4/7.10.3/bin/coreTL`
  
  Generated binary should be run with one arg - path to file you want to parse.
  
# Running test suite
Tests are divided into following groups:
* Parser `test/Test/Parser.hs`
* Typechecker
	* Subtyping `test/Test/Typechecker/Subtyping.hs`
	* Typechecking `test/Test/Typechecker/Typechecker.hs`  

Test suite can be executed with command `stack test`.

  
# TODO
  1. Moving from parsec to parsers+trifecta to improve parser error handling.
