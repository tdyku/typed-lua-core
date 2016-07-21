module Typechecker.Subtype where

import Data.Maybe (isNothing, fromMaybe)

import Types             (F(..), L(..), B(..), P(..), S(..), T(..), E(..), V(..), TType(..))
import AST               (Expr(..), Stm(..), Block(..), LHVal(..), ExprList(..))
import Typechecker.Utils (allT, anyT)
import Text.Show.Pretty  (ppShow    )

class Subtype a where 
  (<?)  :: a -> a -> Bool
  isSub :: [(String, String)] -> a -> a -> Bool

  (<?) = isSub []


instance Subtype F where
    isSub amp (FL  LFalse       ) (FB BBoolean      )  = True
    isSub amp (FL  LTrue        ) (FB BBoolean      )  = True
    isSub amp (FL (LString _  ) ) (FB BString       )  = True
    isSub amp (FL (LInt    _  ) ) (FB BInt          )  = True
    isSub amp (FL (LInt    _  ) ) (FB BNumber       )  = True
    isSub amp (FL (LFloat  _  ) ) (FB BNumber       )  = True
    isSub amp (FB  BInt         ) (FB BNumber       )  = True
    isSub amp  _                   FValue              = True
    isSub amp  _                   FAny                = True
    isSub amp  FAny                _                   = True
    isSub amp (FFunction s1 s2  ) (FFunction s3 s4  )  = isSub amp s3 s1 && isSub amp s2 s4                                              
    isSub amp (FUnion fs        )  x                   = allT $ fmap (\y -> isSub amp y x) fs
    isSub amp  x                  (FUnion fs        )  = anyT $ fmap (isSub amp x) fs
    isSub amp (FVariable x1     ) (FVariable x2     )  = error "Assumption rule" -- if in state then True else False
    isSub amp (FRecursive x1 f1 ) (FRecursive x2 f2 )  = error "Amber rule" -- addToState (x1, x2), f1 <? f2
    isSub amp t1@(FTable ts1 tt1) t2@(FTable ts2 tt2)  | tt1 == Unique && tt2 == Closed = sTable2 amp t1 t2
                                                       | tt1 `elem` [Fixed, Closed] && tt2 == Closed = sTable1 amp t1 t2 
                                                       | tt1 == Unique && tt2 `elem` [Unique, Open, Fixed] = sTable3 amp t1 t2
                                                       | tt1 == Open && tt2 == Closed = sTable4 amp t1 t2
                                                       | tt1 == Open && tt2 `elem` [Open, Fixed] = sTable5 amp t1 t2
                                                       | tt1 == Fixed && tt2 == Fixed = sTable6 amp t1 t2
    isSub amp  x                   y                   = x == y


sTable1, sTable2, sTable3, sTable4, sTable5, sTable6 :: [(String, String)] -> F -> F -> Bool
sTable1 amp (FTable lefts tt1) (FTable rights tt2) =
    let rule (f', v') (f, v) = isSub amp f f' && isSub amp f' f && cSub amp v v'
        firstLaw = fmap (\x -> (fmap (rule x)  lefts)) rights
        forEachExists = allT $ fmap anyT firstLaw
    in  forEachExists


sTable2 amp (FTable ts1 tt1) (FTable ts2 tt2) = 
    let rule1 (f,v) (f',v') = if isSub amp f f' then uSub amp v v' else True
        rule2 (f',_) (f,_) = isSub amp f f'
        condSubtyping1 = fmap allT $ fmap (\x -> fmap (rule1 x) ts2) ts1
        condSubtyping2 = fmap (\(f',v') -> let subResult = fmap (rule2 (f', v')) ts1
                                           in if all (== False) subResult then oSub amp (VF FNil) v' else True) ts2
    in allT condSubtyping1 && allT condSubtyping2
 

sTable3 amp (FTable ts1 tt1) (FTable ts2 tt2) = 
    let rule1 (f,v) (f',v') = isSub amp f f' && uSub amp v v'
        rule2 (f',_) (f,_) = isSub amp f f'
        condSubtyping1 = fmap anyT $ fmap (\x -> fmap (rule1 x) ts2) ts1
        condSubtyping2 = fmap (\(f',v') -> let subResult = fmap (rule2 (f', v')) ts1
                                           in if all (== False) subResult then oSub amp (VF FNil) v' else True) ts2
    in allT condSubtyping1 && allT condSubtyping2


sTable4 amp (FTable ts1 tt1) (FTable ts2 tt2) = 
    let rule1 (f,v) (f',v') = if isSub amp f f' then cSub amp v v' else True
        rule2 (f',_) (f,_) = isSub amp f f'
        condSubtyping1 = fmap allT $ fmap (\x -> fmap (rule1 x) ts2) ts1
        condSubtyping2 = fmap (\(f',v') -> let subResult = fmap (rule2 (f', v')) ts1
                                           in if all (== False) subResult then oSub amp (VF FNil) v' else True) ts2
    in allT condSubtyping1 && allT condSubtyping2


sTable5 amp (FTable ts1 tt1) (FTable ts2 tt2) = 
    let rule1 (f,v) (f',v') = isSub amp f f' && cSub amp v v'
        rule2 (f',_) (f,_) = isSub amp f f'
        condSubtyping1 = fmap allT $ fmap (\x -> fmap (rule1 x) ts2) ts1
        condSubtyping2 = fmap (\(f',v') -> let subResult = fmap (rule2 (f', v')) ts1
                                           in if all (== False) subResult then oSub amp (VF FNil) v' else True) ts2
    in anyT condSubtyping1 && allT condSubtyping2


sTable6 amp (FTable ts1 tt1) (FTable ts2 tt2) = 
    let rule1 (f,v) (f',v') = isSub amp f f' && isSub amp f' f && cSub amp v  v'
        rule2 (f,v) (f',v') = isSub amp f f' && isSub amp f' f && cSub amp v' v
        condSubtyping1 = fmap allT $ fmap (\x -> fmap (rule1 x) ts2) ts1
        condSubtyping2 = fmap allT $ fmap (\x -> fmap (rule2 x) ts1) ts2
    in anyT condSubtyping1 && anyT condSubtyping2

instance Subtype S where
    isSub amp (SUnion ss) (SP p)  = allT $ fmap (\x -> isSub amp x p) ss
    isSub amp (SP p ) (SUnion ss) = anyT $ fmap (isSub amp p ) ss
    isSub amp (SP p1) (SP p2    ) = isSub amp p1 p2


instance Subtype P where 
    isSub amp (P fs1 mf1) (P fs2 mf2) = allT $ fmap (uncurry (isSub amp)) (tupleZip fs1 mf1 fs2 mf2)


tupleZip ls l rs r | length ls == length rs = zip ls rs
                   | length ls < length rs = zip (ls ++ repeat (fromMaybe FNil l)) rs
                   | otherwise = zip ls (rs ++ repeat (fromMaybe FNil r))


instance Subtype T where
    isSub amp (TF f1        ) (TF f2        ) = isSub amp f1 f2
    isSub amp (TFilter x1 y1) (TFilter x2 y2) = (x1 == x2) && (y1 == y2)
    isSub amp (TProj x1 i1  ) (TProj x2 i2  ) = (x1 == x2) && (i1 == i2 )


instance Subtype E where
    isSub amp (E ts1 mt1) (E ts2 mt2) = allT $ fmap (uncurry (isSub amp)) (tupleZipE ts1 mt1 ts2 mt2)


tupleZipE ls l rs r | length ls == length rs = zip ls rs
                    | length ls < length rs = zip (ls ++ repeat (fromMaybe (TF FNil) l)) rs
                    | otherwise = zip ls (rs ++ repeat (fromMaybe (TF FNil) r))


-- subtyping for V
cSub, oSub, uSub :: [(String, String)] -> V -> V -> Bool
cSub amp (VF f1) (VF f2) = f1 <? f2 && f2 <? f1
cSub amp (VConst f1) (VConst f2) = f1 <? f2
cSub amp (VF f1) (VConst f2) = f1 <? f2

uSub amp (VF f1) (VF f2) = f1 <? f2 
uSub amp (VConst f1) (VConst f2) = f1 <? f2
uSub amp (VConst f1) (VF f2) = f1 <? f2
uSub amp (VF f1) (VConst f2) = f1 <? f2

oSub amp (VF FNil) (VF f) = FNil <? f
oSub amp (VF FNil) (VConst f) = FNil <? f



