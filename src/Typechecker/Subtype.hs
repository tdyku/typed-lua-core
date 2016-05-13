module Typechecker.Subtype where

import Types (F(..), L(..), B(..), P(..), S)
import AST (Expr(..), Stm(..), Block(..), LHVal(..), ExprList(..))

allT = all (\x -> x == True)
anyT = any (\x -> x == True)

class Subtype a where 
  (<?) :: a -> a -> Bool


instance Subtype F where
    (FL LFalse     ) <? (FB BBoolean) = True
    (FL LTrue      ) <? (FB BBoolean) = True
    (FL (LString _)) <? (FB BString ) = True
    (FL (LInt    _)) <? (FB BInt    ) = True
    (FL (LInt    _)) <? (FB BNumber ) = True
    (FL (LFloat  _)) <? (FB BNumber ) = True
    (FB (BInt     )) <? (FB BNumber ) = True
    _                <?  FValue       = True
    _                <?  FAny         = True
    FAny             <?  _            = True
    FUnion fs        <? x             = if allT $ fmap (\f -> f <? x) fs then True else False
    x                <? FUnion fs     = if anyT $ fmap (\f -> x <? f) fs then True else False
    x                <? y             = if x == y then True else False