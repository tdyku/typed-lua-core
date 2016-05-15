module Typechecker.Subtype where

import Data.Maybe (isNothing, fromJust)

import Types (F(..), L(..), B(..), P(..), S(..))
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


instance Subtype S where
    SUnion ss <? SP p      = if allT $ fmap (\s -> s <? p) ss then True else False
    SP p      <? SUnion ss = if anyT $ fmap (\s -> p <? s) ss then True else False
    SP p1     <? SP p2     = p1 <? p2


instance Subtype P where 
    P fs1 mf1 <? P fs2 mf2 = if allT $ fmap (\(a,b) -> a <? b) (tupleZip fs1 mf1 fs2 mf2) then True else False


tupleZip ls l rs r | length ls == length rs = zip ls rs
                   | length ls < length rs = zip (ls ++ repeat (if isNothing l then FNil else fromJust l)) rs
                   | otherwise = zip ls (rs ++ repeat (if isNothing r then FNil else fromJust r))