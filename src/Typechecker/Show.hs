module Typechecker.Show where

import Data.List (intercalate)

import Types


class TypeShow a where
    tShow :: a -> String



instance TypeShow F where
    tShow (FL l) = tShow l
    tShow (FB b) = tShow b
    tShow FNil = "nil"
    tShow FValue = "value"
    tShow FAny = "any"
    tShow FSelf = "self"
    tShow (FUnion fs) = "(" ++ intercalate "|" (fmap tShow fs) ++ ")"
    tShow (FFunction s1 s2) = tShow s1 ++ "->" ++ tShow s2
    tShow (FTable tts tt) = "{" ++ intercalate ", " (fmap (\(x,y) -> tShow x ++ ":" ++ tShow y) tts) ++ "}_" ++ tShow tt
    tShow (FVariable x) = x
    tShow (FRecursive x f) = "u" ++ x ++ "." ++ tShow f


instance TypeShow L where
    tShow LFalse = "false"
    tShow LTrue = "true"
    tShow (LInt i) = show i
    tShow (LFloat f) = show f
    tShow (LString s) = "\"" ++ s ++ "\""
    

instance TypeShow B where
    tShow BBoolean = "boolean"
    tShow BInt = "integer"
    tShow BNumber = "number"
    tShow BString = "string"
    

instance TypeShow V where
    tShow (VF f) = "nonconst " ++ tShow f
    tShow (VConst f) = "const " ++ tShow f


instance TypeShow TType where
    tShow Unique = "unique"
    tShow Open = "open"
    tShow Closed = "closed"
    tShow Fixed = "fixed"

    
instance TypeShow S where
    tShow (SP p) = tShow p
    tShow (SUnion ps) = intercalate "|" (fmap tShow ps)


instance TypeShow P where
    tShow (P fs mF) = "(" ++ intercalate "," (fmap tShow fs) ++ maybe "" (\x ->  "," ++ tShow x ++ "*") mF  ++ ")"


instance TypeShow T where
    tShow (TF f) = tShow f
    tShow (TFilter f1 f2) = "phi(" ++ tShow f1 ++ "," ++ tShow f2 ++ ")"
    tShow (TProj x i) = "pi(" ++ show x ++ "," ++ show i ++ ")" 


instance TypeShow E where
    tShow (E ts mT) = "(" ++ intercalate "," (fmap tShow ts) ++ maybe "" (\x ->  "," ++ tShow x ++ "*") mT  ++ ")"


instance TypeShow R where
    tShow (RF f) = "r(" ++ tShow f ++ ")"
    tShow RVoid = "void"    