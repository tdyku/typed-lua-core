module Test.Typechecker.Subtyping where

import Typechecker.Subtype
import Types
import Test.Hspec

literalsTest :: Spec
literalsTest = describe "Literal types" $ do
        it "true and false are subtype of boolean" $
            (FL LFalse) <? (FB BBoolean) && (FL LTrue) <? (FB BBoolean)

        it "boolean is not be subtype of true or false" $
            ((FB BBoolean) <? (FL LFalse) == False) && ((FB BBoolean) <? (FL LFalse) == False)

        it "string literal is subtype of string" $
            (FL (LString "literal")) <? (FB BString)

        it "string is not subtype of string literal" $
            not $ (FB BString) <? (FL (LString "literal"))

        it "integer literal is subtype of int" $
            (FL (LInt 1)) <? (FB BInt)

        it "float literal is subtype of number" $
            (FL (LFloat 1.0)) <? (FB BNumber)
        
        it "integer literal is subtype of number" $
            (FL (LInt 1)) <? (FB BNumber)


basicTest :: Spec
basicTest = describe "Basic types" $ do
        it "integer is subtype of number" $
            FB BInt <? FB BNumber

        it "nil is subtype of nil" $
            FNil <? FNil

        it "anything is subtype of value" $
            (FL (LFloat 1.0)) <? FValue    

        it "any is subtype of any" $
            FAny <? FAny

        it "self is subtype of self" $
            FSelf <? FSelf

        it "member of union is subtype of this union" $
            FL (LInt 5) <? FUnion [FL (LString "a"), FL (LInt 5)]


        it "functions subtyping" $ 
            let f1 = FFunction (SP $ P [FB BInt] Nothing) (SP $ P [FL (LString "a")] Nothing)
                f2 = FFunction (SP $ P [FL (LInt 3)] Nothing) (SP $ P [FB BString] Nothing)
            in f1 <? f2

        it "pair subtyping" $
            let p1 = P [FL (LString "a"), FL LTrue, FB BInt] Nothing
                p2 = P [FB BString, FB BBoolean, FB BInt] Nothing
            in p1 <? p2

        it "vararg" $
            let p1 = P [FB BString, FL (LString "a")] Nothing
                p2 = P [] (Just $ FB BString)
            in p1 <? p2

tableTest :: Spec
tableTest = describe "Table types" $ do
    it "fixed/closed < closed" $
        let tfc = FTable [(FB BString, VF $ FNil), (FB BInt, VF $ FL LTrue)] Fixed
            tc  = FTable [(FB BString, VF $ FUnion [FNil]), (FB BInt, VF $ FL LTrue)] Closed
        in tfc <? tc
    it "unique < closed" True
    it "unique < unique/open/fixed" True
    it "open < closed" True
    it "open < open/fixed" True
    it "fixed < fixed" True


recursiveTest :: Spec
recursiveTest = describe "Recursive types" $ do
    it "assumption for given x1 and x2" $
        let v1 = FVariable "x1"
            v2 = FVariable "x2"
        in isSub [("x1", "x2")] v1 v2

    it "amber rule" $
        let r1 = FRecursive "x1" (FVariable "x1")
            r2 = FRecursive "x2" (FVariable "x2")
        in r1 <? r2

    it "folding" $ 
        let l = FTable [(FL (LString "next"), VF FNil)] Unique
            r = FRecursive "x" (FTable [(FL (LString "next"), VF $ FUnion [FVariable "x", FNil])] Unique)
        in l <? r

anyTest :: Spec
anyTest = describe "Dynamic type any" $ do
    it "f is subtype of any" $
        FFunction (SP $ P [FB BInt] Nothing) (SP $ P [FL (LString "a")] Nothing) <? FAny
    it "any is subtype of f" $
        FAny <? FTable [(FB BString, VF $ FUnion [FNil]), (FB BInt, VF $ FL LTrue)] Closed