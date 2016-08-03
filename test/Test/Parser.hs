module Test.Parser where

import Text.Trifecta.Parser (parseString)
import Text.Trifecta.Result (Result(..))

import Parser.Code (pExpr, pExprList, pA, pManyStm)
import Parser.Types (pF, pS)
import AST
import Types


import Test.Hspec

parserTest :: Spec
parserTest = do describe "Expressions parsing" $ do
                    it "BinOp expression parsing" $ 
                      (parseString pExpr mempty "1 / 2 + 3") 
                      `shouldBe` 
                      (Success $ ExpABinOp Add (ExpABinOp Div (ExpInt 1) (ExpInt 2)) (ExpInt 3)) 
  
  
                    it "UnOp Parsing" $ 
                        (parseString pExpr mempty "#\"SomeWord\"") 
                        `shouldBe`
                        (Success $ ExpUnaryOp Hash (ExpString "SomeWord")) 
  
  
  
                    it "ExprList with application" $ 
                        (parseString pExprList mempty "1, \"test\", foo(2, \"arg2\")") 
                        `shouldBe` 
                        Success (ExprList
                                [ ExpInt 1 , ExpString "test" ]
                                (Just
                                   (FunAppl
                                      (ExpVar "foo")
                                      (ExprList
                                         [ ExpInt 2 ,  (ExpString "arg2") ]
                                         Nothing))))
  
  
                    it "ExprList with vararg" $ 
                        (parseString pExprList mempty "1, \"test\", ...")    
                        `shouldBe` 
                        Success (ExprList [ ExpInt 1 , ExpString "test" ] (Just VarArg))
  
  
                    it "Function call" $
                        (parseString pA mempty "foo(1, \"15\", someVar, foo2())")    
                        `shouldBe` 
                        Success  (FunAppl
                                 (ExpVar "foo")
                                (ExprList
                                   [ ExpInt 1
                                   , ExpString "15"
                                   , (ExpVar "someVar")
                                   ]
                                   (Just
                                      (FunAppl
                                         (ExpVar "foo2")
                                         (ExprList [] Nothing)))))
                    
  
                    it "Method invocation" $
                        (parseString pA mempty "tab:foo(42.1)")
                        `shouldBe` 
                        Success (MthdAppl
                            (ExpVar "tab")
                            "foo"
                          (ExprList [ ExpFloat 42.1 ] Nothing))


                    it "Function declaration" $
                        (parseString pExpr mempty "fun (a:number, b:string):(number) a = 3; return a")
                        `shouldBe`
                        Success (
                         ExpFunDecl
                         (ParamList [ ( "a" , FB BNumber ) , ( "b" , FB BString ) ] Nothing)
                         (SP (P [ FB BNumber ] Nothing))
                         (Block
                            [ StmAssign [ IdVal "a" ] (ExprList [ ExpInt 3 ] Nothing)
                            , StmReturn (ExprList [ ExpVar "a" ] Nothing)
                            ])
            
                        )


                    it "Table constructor" $ 
                        (parseString pExpr mempty "{[\"one\"]=1, [\"two\"] = 2.2, [3] = \"three\"}")
                        `shouldBe`
                        Success (ExpTableConstructor [(ExpString "one", ExpInt 1), (ExpString "two", ExpFloat 2.2), (ExpInt 3, ExpString "three")] Nothing)

                    
                    it "Table constructor with mArgs" $ 
                        (parseString pExpr mempty "{[\"one\"]=1, tab:foo(1,...)}")
                        `shouldBe`
                        Success (ExpTableConstructor [(ExpString "one", ExpInt 1)] (Just $ MthdAppl (ExpVar "tab") "foo" (ExprList [ExpInt 1] (Just VarArg))))


                describe "Parsing types" $ do
                    it "True literal" $
                        (parseString pF mempty "true")
                        `shouldBe`
                        Success (FL LTrue)


                    it "String literal" $
                        (parseString pF mempty "\"stringLit\"")
                        `shouldBe`
                        Success (FL (LString "stringLit"))

                    it "Integer literal" $
                        (parseString pF mempty "128")
                        `shouldBe`
                        Success (FL (LInt 128))


                    it "Float literal" $
                        (parseString pF mempty "128.1")
                        `shouldBe`
                        Success (FL (LFloat 128.1))


                    it "Union" $
                        (parseString pF mempty "(self|any|value|nil|128)")
                        `shouldBe`
                        Success (FUnion [FSelf, FAny, FValue, FNil, FL (LInt 128)])


                    it "Function" $
                        (parseString pF mempty "(integer, boolean, false, number*) -> (string, string, nil*)")
                        `shouldBe`
                        Success (FFunction (SP $ P [FB BInt, FB BBoolean, FL LFalse] (Just (FB BNumber))) (SP $ P [FB BString, FB BString] (Just FNil)))


                    it "Union of tuples" $
                        (parseString pS mempty "((number*)|(string, string)|(number))")
                        `shouldBe`
                        Success (SUnion [P [] (Just (FB BNumber)), P [FB BString, FB BString] Nothing, P [FB BNumber]Nothing] )


                    it "Recursive declaration" $
                        (parseString pF mempty "ux.{number:x}")
                        `shouldBe`
                        Success (FRecursive "x" (FTable [(FB BNumber, VF $ FVariable "x")] Unique))


                    it "Empty table of fixed type" $ 
                        (parseString pF mempty "{}_fixed")
                        `shouldBe`
                        Success (FTable [] Fixed)

                    it "Table" $ 
                        (parseString pF mempty "{1:const 2, boolean:number, any:(any)->(value)}_open")
                        `shouldBe`
                        Success ( FTable
                                            [ ( FL (LInt 1) , VConst (FL (LInt 2)) )
                                            , ( FB BBoolean , VF (FB BNumber) )
                                            , ( FAny
                                              , VF
                                                  (FFunction (SP (P [ FAny ] Nothing)) (SP (P [ FValue ] Nothing)))
                                              )
                                            ]
                                            Open
                                )




                describe "Parsing statements" $ do
                    it "if statement" $
                        (parseString pManyStm mempty (unlines ["if 1 < 3" 
                                                              ,"then a = 1;" 
                                                              ,"     b = 2;"
                                                              ,"     c = 3"
                                                              ,"else a = b * c"]))
                        `shouldBe`
                        Success (Block
                                  [ StmIf
                                      (ExpABinOp LessThan (ExpInt 1) (ExpInt 3))
                                      (Block
                                         [ StmAssign [ IdVal "a" ] (ExprList [ ExpInt 1 ] Nothing)
                                         , StmAssign [ IdVal "b" ] (ExprList [ ExpInt 2 ] Nothing)
                                         , StmAssign [ IdVal "c" ] (ExprList [ ExpInt 3 ] Nothing)
                                         ])
                                      (Block
                                         [ StmAssign [ IdVal "a" ] (ExprList [ ExpVar "b" ] Nothing) ])
                                  ]
                                )


                    it "while + method declaration" $
                        (parseString pManyStm mempty (unlines ["while not 1 < 5 do" 
                                                              ,"    local tab = {} in" 
                                                              ,"     fun tab:foo(x:integer):(integer) return x + 10"]))
                        `shouldBe`
                        Success (Block
                                      [ StmWhile
                                          (ExpUnaryOp Not (ExpABinOp LessThan (ExpInt 1) (ExpInt 5)))
                                          (Block
                                             [ StmVarDecl
                                                 [ "tab" ]
                                                 (ExprList [ ExpTableConstructor [] Nothing ] Nothing)
                                                 (Block
                                                    [ StmMthdDecl
                                                        "tab"
                                                        "foo"
                                                        (ParamList [ ( "x" , FB BInt ) ] Nothing)
                                                        (SP (P [ FB BInt ] Nothing))
                                                        (Block
                                                           [ StmReturn
                                                               (ExprList [ ExpABinOp Add (ExpVar "x") (ExpInt 10) ] Nothing)
                                                           ])
                                                    ])
                                             ])
                                      ]
                                    
                                    )


                    it "typed local declaration + fun definition + table refinement" $
                        (parseString pManyStm mempty (unlines ["local b:{}_unique, foo:(number) -> (number) " 
                                                              ,"    = {}, fun (a:number):(number) return a in" 
                                                              ,"        b[\"x\"] <string> = \"bar\""]))
                        `shouldBe`
                        Success (Block
                                  [ StmTypedVarDecl
                                      [ ( "b" , FTable [] Unique )
                                      , ( "foo"
                                        , FFunction
                                            (SP (P [ FB BNumber ] Nothing)) (SP (P [ FB BNumber ] Nothing))
                                        )
                                      ]
                                      (ExprList
                                         [ ExpTableConstructor [] Nothing
                                         , ExpFunDecl
                                             (ParamList [ ( "a" , FB BNumber ) ] Nothing)
                                             (SP (P [ FB BNumber ] Nothing))
                                             (Block [ StmReturn (ExprList [ ExpVar "a" ] Nothing) ])
                                         ]
                                         Nothing)
                                      (Block
                                         [ StmAssign
                                             [ TypeCoercionVal "b" (ExpString "x") (VF (FB BString)) ]
                                             (ExprList [ ExpString "bar" ] Nothing)
                                         ])
                                  ]
                                )

                





instance Eq a => Eq (Result a) where
    Success a == Success b = a == b
    (==) _ _ = False