module Typechecker.Subtype where

import Data.Maybe (isNothing, fromJust)

import Types      (F(..), L(..), B(..), P(..), S(..), T(..), E(..), V(..))
import AST        (Expr(..), Stm(..), Block(..), LHVal(..), ExprList(..))


allT = all (== True)
anyT = any (== True)


class Subtype a where 
  (<?) :: a -> a -> Bool


-- Do we need it?
--class CSubtype a where    -- depth subtyping of table fields - <c
--  (<!) :: a -> a -> Bool


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
    FUnion fs        <? x             = allT $ fmap (<? x) fs
    x                <? FUnion fs     = anyT $ fmap (x <?) fs
    x                <? y             = x == y


instance Subtype V where
    VF f1 <? VF f2 = f1 <? f2 && f2 <? f1
    VConst f1 <? VConst f2 = f1 <? f2
    VF f1 <? VConst f2 = f1 <? f2

instance Subtype S where
    SUnion ss <? SP p      = allT $ fmap (<? p) ss
    SP p      <? SUnion ss = anyT $ fmap (p <?) ss
    SP p1     <? SP p2     = p1 <? p2


instance Subtype P where 
    P fs1 mf1 <? P fs2 mf2 = allT $ fmap (uncurry (<?)) (tupleZip fs1 mf1 fs2 mf2)


tupleZip ls l rs r | length ls == length rs = zip ls rs
                   | length ls < length rs = zip (ls ++ repeat (if isNothing l then FNil else fromJust l)) rs
                   | otherwise = zip ls (rs ++ repeat (if isNothing r then FNil else fromJust r))


instance Subtype T where
    TF f1         <? TF f2         = f1 <? f2
    TFilter x1 y1 <? TFilter x2 y2 = (x1 == x2) && (y1 == y2)
    TProj x1 i1   <? TProj x2 i2   = (x1 == x2) && (i1 == i2 )


instance Subtype E where
    E ts1 mt1 <? E ts2 mt2 = allT $ fmap (uncurry (<?)) (tupleZipE ts1 mt1 ts2 mt2)


tupleZipE ls l rs r | length ls == length rs = zip ls rs
                   | length ls < length rs = zip (ls ++ repeat (if isNothing l then (TF FNil) else fromJust l)) rs
                   | otherwise = zip ls (rs ++ repeat (if isNothing r then (TF FNil) else fromJust r))

