{-# LANGUAGE LambdaCase #-}

module Test.Typechecker.Typechecker where


import Test.Hspec

import Test.Typechecker.Subtyping
import Test.Typechecker.Utils   


isCorrect :: IO (Either String ()) -> Expectation
isCorrect v = (v >>= \case
    Right () -> return True
    Left _ -> return False)
    `shouldReturn`
    True


shouldFail :: IO (Either String ()) -> Expectation
shouldFail v = (v >>= \case
    Right () -> return False
    Left _ -> return True)
    `shouldReturn`
    True



typecheckerTest :: Spec
typecheckerTest = do 
    describe "Subtyping" $ do
        literalsTest
        basicTest
        tableTest
        recursiveTest
        anyTest
    describe "Typechecking" $ do
        binOps
        statements
        tables
        objects
        recursion
        metatable



binOps :: Spec
binOps = describe "Simple expressions" $ do
    it "BinOps" $ isCorrect $ typeCheck source1 
    it "UnOps" $ isCorrect $ typeCheck source2
    it "Global variables" $ isCorrect $ typeCheck source3
    where path = "examples/simple/"
          source1 = path ++ "source1.tlc"
          source2 = path ++ "source2.tlc"
          source3 = path ++ "source3.tlc"


statements :: Spec
statements = describe "Statements" $ do
    it "Projection types" $ isCorrect $ typeCheck source
    where path = "examples/statements/"
          source = path ++ "source1.tlc"

tables :: Spec
tables = describe "Tables" $ do
    it "Simple table construction" $ isCorrect $ typeCheck source1
    it "Table refinement" $ isCorrect $ typeCheck source2
    it "Aliasing unique tables" $ shouldFail $ typeCheck source3
    it "Types coercion" $ isCorrect $ typeCheck source4
    it "Unique-closed subtyping" $ isCorrect $ typeCheck source5
    where path = "examples/tables/"
          source1 = path ++ "source1.tlc"
          source2 = path ++ "source2.tlc"
          source3 = path ++ "source3.tlc"
          source4 = path ++ "source4.tlc"
          source5 = path ++ "source5.tlc"

objects :: Spec
objects = describe "Methods and objects" $ do
    it "Basic method typechecking" $ isCorrect $ typeCheck source1  
    it "Adding new method - subtype of another one" $ isCorrect $ typeCheck source2
    where path = "examples/objects/"
          source1 = path ++ "source1.tlc"
          source2 = path ++ "source2.tlc"

recursion :: Spec
recursion = describe "Recursive types" $
    it "Recursive declaration" $ isCorrect $ typeCheck source1
    where path = "examples/recursion/"
          source1 = path ++ "source1.tlc"



metatable :: Spec
metatable = describe "Metatable tests" $
      it "Basic metatable test" $ isCorrect $ typeCheck source1
      where path = "examples/metatable/"
            source1 = path ++ "source1.tlc"
