module Typechecker.AuxFuns where

import Control.Monad.State      (State, StateT, liftIO, get, put, evalStateT)
import Control.Monad.Except     (ExceptT, throwError, runExceptT)
import Control.Lens
import Types                    (F(..), L(..), B(..), T(..), R(..), P(..), S(..), E(..), TType(..), V(..))
import Data.Map                 (lookup)
import Prelude                  hiding (pi, lookup)
import Data.Maybe               (fromJust)
import Typechecker.Subtype      ((<?))
import Typechecker.Utils        (anyT, allT)



vt :: F -> V -> V
vt (FL _) (VConst v) = VConst . fix $ v
vt (FL _) (VF v) = VF . fix $ v
vt _ (VF f2) = VF . nilF . fix $ f2
vt _ (VConst f2) = VConst . nilF . fix $ f2  

fix :: F -> F
fix (FUnion fs) = FUnion $ fmap fix fs
fix (FTable ts Unique) = FTable ts Fixed
fix (FTable ts Open  ) = FTable ts Fixed
fix f = f

isConst :: V -> Bool
isConst (VF _) = False
isConst (VConst _) = True


rconst :: V -> F
rconst (VF f) = f
rconst (VConst f) = f


wf :: F -> Bool
wf (FTable tList _) = 
    let fTypesEnum = zip [0..] (fmap fst tList)
        vTypes = fmap (unwrapV . snd) tList
        productList = [(x,y) | x <- fTypesEnum, y <- fTypesEnum] 
        isSubtype ((n1, f1), (n2, f2)) = n1 /= n2 && f1 <? f2  -- we need all false
        negList es = fmap not es
        checkedSubtypes = negList $ fmap isSubtype productList
        
    in (allT checkedSubtypes) && (allT $ fmap (not . (tag Unique)) vTypes) && (allT $ fmap (not . (tag Open)) vTypes)  


unwrapV :: V -> F
unwrapV (VF f) = f
unwrapV (VConst f) = f 

tag :: TType -> F -> Bool
tag t2 (FTable _ t1)  = t1 == t2
tag t (FUnion fs) = anyT $ fmap (tag t) fs
tag _ _ = False

infer :: E -> Int -> T
infer (E ts ms) i 
    | i > length ts = case ms of
                        Just mt -> general . nil $ mt
                        Nothing -> TF FNil
    | otherwise     = general $ ts !! i



nil :: T -> T
nil t@(TF f)
    | FNil <? f = t
    | otherwise = TF $ FUnion [f, FNil]

nilF :: F -> F
nilF f 
    | FNil <? f = f
    | otherwise = FUnion [f, FNil]


generalF (FL LFalse)      = FB BBoolean
generalF (FL LTrue)       = FB BBoolean
generalF (FL (LInt _))    = FB BInt
generalF (FL (LFloat _))  = FB BNumber
generalF (FL (LString _)) = FB BString
generalF (FUnion fs)      = FUnion $ fmap generalF fs
generalF f                = f

general :: T -> T
general (TF x) = TF $ generalF x
general x      = x

fit, fot :: F -> F -> R
fit (FUnion [f1, f2]) f3  
    | fit f2 f3 == RVoid = fit f1 f3
    | fit f1 f3 == RVoid = fit f2 f3
    | otherwise = let (RF f13) = fit f1 f3
                      (RF f23) = fit f2 f3
                  in RF (FUnion [f13, f23])
fit f1 f2 
    | f1 <? f2 && f2 <? f1 = RF f1
    | otherwise = RVoid

fot (FUnion [f1, f2]) f3  
    | fot f2 f3 == RVoid = fot f1 f3
    | fot f1 f3 == RVoid = fot f2 f3
    | otherwise = let (RF f13) = fot f1 f3
                      (RF f23) = fot f2 f3
                  in RF (FUnion [f13, f23])
fot f1 f2 
    | f1 <? f2 && f2 <? f1 = RVoid
    | otherwise = RF f1


proj :: S -> Int -> F
proj (SUnion ps) i = FUnion $ fmap (\x -> proj (SP x) i) ps
proj (SP (P fs mf)) i
    | i <= length fs = fs !! i
    | otherwise = if mf == Nothing then FNil else nilF  . fromJust $ mf 

fipt,fopt :: S -> F -> Int -> S
fipt s@(SP _) discF i = let projRes = fit (proj s i) discF
                        in if projRes == RVoid then SUnion [] else s 
fipt s@(SUnion ps) discF i = let fP = filter (\x -> (fit (proj (SP x) i) discF) /= RVoid ) ps
                             in SUnion fP

fopt s@(SP _) discF i = let projRes = fot (proj s i) discF
                        in if projRes == RVoid then SUnion [] else s 
fopt s@(SUnion ps) discF i = let fP = filter (\x -> (fot (proj (SP x) i) discF) /= RVoid ) ps
                             in SUnion fP