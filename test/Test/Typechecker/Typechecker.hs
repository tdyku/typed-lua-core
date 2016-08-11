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
    Right () -> return True
    Left _ -> return False)
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
        objects



binOps :: Spec
binOps = describe "Simple expressions" $ do
    it "BinOps" $ isCorrect $ typeCheck source1 
    it "UnOps" $ isCorrect $ typeCheck source2
    it "Global variables" $ isCorrect $ typeCheck source3
    where source1 = unlines ["local res:number, a:integer, b:number = 0, 1, 0.5 in"
                  , "res = a + b"
                  ]

          source2 = unlines ["local len:integer, word:string = 0, \"testingWord\" in"
                            , "len = #word"
                            ]
          source3 = unlines [ "local ENV:{\"b\":integer}_unique = {[\"b\"]=0} in"    
                            , "    local a = 2 in" 
                            , "        b = a + 2"
                            ]


statements :: Spec
statements = describe "Statements" $ do
    it "Projection types" $ isCorrect $ typeCheck source
    where source = unlines [ "local error,result = \"\",1 in"
                           , "local idiv = fun(dividend:integer, divisor:integer): ((integer, integer)|(nil, string))"
                           , "        local q,r = 0,0 in"
                           , "            if divisor == 0 then"
                           , "                return nil, \"divZero\""
                           , "            else"
                           , "                r = dividend / divisor;"
                           , "                q = dividend % r;"
                           , "                return q, r "
                           , "in"
                           , "    local p,q = idiv(10,2) in"
                           , "        if p then result = q else error =  q"
                           ] 


tables :: Spec
tables = describe "Tables" $ do
    it "Simple table construction" $ isCorrect $ typeCheck source0
    it "Table refinement" $ isCorrect $ typeCheck source1
    it "Aliasing unique tables" $ shouldFail $ typeCheck source2
    it "Types coercion" $ isCorrect $ typeCheck source3
    where source0 = unlines [ "local idiv = fun(dividend:integer, divisor:integer): ((integer, integer)|(nil, string))"
                            , "    skip" 
                            , "in"
                            , "    local a = {[\"x\"] = 1, [\"y\"] = 2, idiv(10, 5)} in"
                            , "        skip"
                            ]
          source1 = unlines [ "local person = {} in"
                            , "person[\"firstname\"] <string> = \"Lou\"; "
                            , "person[\"lastname\"] <string> = \"Reed\""
                            ]
          source2 = unlines [ "local a : {}_unique = {} in"
                            , "local b : {}_open = a in"
                            , "    a[\"x\"] <string> = \"foo\";"
                            , "    b[\"x\"] <integer> = 1"
                            ]
          source3 = unlines [ "local a : {}_unique = {} in"
                            , "a[\"x\"] <string> = \"foo\";"
                            , "a[\"y\"] <string> = \"bar\";"
                            , "local b : {\"x\" : string, \"y\" : (string|nil)}_closed = <{\"x\" : string, \"y\" : (string|nil)}_open> a in "
                            , "    a[\"z\"] <integer> = 1"
                            ]

recursion :: Spec
recursion = describe "Recursive types" $
    it "Recursive declaration" $ isCorrect $ typeCheck source0
    where source0 = unlines [ "rec a : ux.{\"next\":(x|nil)} = {[\"next\"]={[\"next\"] = nil  }} in"
                            , "    skip"
                            ]



metatable :: Spec
metatable = describe "Metatable tests" $
      it "Basic metatable test" $ isCorrect $ typeCheck source0
      where source0 = unlines [ "local tab = {} in"
                              , "    fun tab:foo():(number) |setmetatable({}, {[\"index\"] = tab})|; return 0"
                              ]


objects :: Spec
objects = describe "Methods and objects" $ do
    it "Basic method typechecking" $ isCorrect $ typeCheck source0  
    it "Adding new method - subtype of other one" $ isCorrect $ typeCheck source1
    where source0 = unlines [ "local tab:{number:string}_unique = {[1] = \"jeden\"} in"
                            , "    fun tab:a(age:number):(number) return age"
                            ]
          source1 = unlines [ "local tab:{\"a\":(number) -> (number)}_unique = { [\"a\"] = fun(x:number):(number) return x } in"
                            , "fun tab:a(age:number):(number) return age"
                            ]