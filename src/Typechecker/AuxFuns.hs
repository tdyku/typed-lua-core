module Typechecker.AuxFuns where

import Control.Monad.State      (State, StateT, liftIO, get, put, evalStateT)
import Control.Monad.Except     (ExceptT, throwError, runExceptT)
import Control.Lens
import Data.Map                 (lookup, Map, mapMaybeWithKey)
import Prelude                  hiding (pi, lookup)
import Data.Maybe               (fromJust, isNothing)
import Data.List                (nub)

import Typechecker.Subtype      ((<?))
import Typechecker.Utils        (anyT, allT)
import Types                    (F(..), L(..), B(..), T(..), R(..), P(..), S(..), E(..), TType(..), V(..))
import AST                      (LHVal(..), Stm(..), Expr(..), Block(..), ExprList(..))



-- catches everything from left side
fav :: [LHVal] -> Stm -> [LHVal]
fav elems (StmAssign e1 _) = elems ++ e1
fav elems (StmWhile _ (Block stms)) = elems ++ (concatMap (fav []) stms)                      
fav elems (StmIf _ (Block b1) (Block b2)) = elems ++ (concatMap (fav []) b1) ++ (concatMap (fav []) b2)                   
fav elems (StmTypedVarDecl ids _ (Block b1)) = elems ++ ((IdVal . fst) <$> ids) ++ (concatMap (fav []) b1) 
fav elems (StmVarDecl ids _ (Block b1)) = elems ++ (IdVal <$> ids) ++ (concatMap (fav []) b1)           
fav elems (StmRecDecl (id,_) _ (Block b1)) = elems ++ [IdVal id] ++  (concatMap (fav []) b1)              
fav elems _ = elems      


-- catches everything from right side
frv :: [String] -> Stm -> [String]
frv elems (StmAssign _ (ExprList ex1 _)) = elems ++ (getId <$> filter isVar ex1)
frv elems (StmWhile _ (Block stms)) = elems ++ (concatMap (frv []) stms)                      
frv elems (StmIf _ (Block b1) (Block b2)) = elems ++ (concatMap (frv []) b1) ++ (concatMap (frv []) b2)                   
frv elems (StmTypedVarDecl _ (ExprList ex1 _) (Block b1)) = elems ++ (getId <$> filter isVar ex1) ++ (concatMap (frv []) b1) 
frv elems (StmVarDecl _ (ExprList ex1 _) (Block b1)) = elems ++ (getId <$> filter isVar ex1) ++ (concatMap (frv []) b1)           
frv elems (StmRecDecl _ ex1 (Block b1)) = elems ++ (getId <$> filter isVar [ex1]) ++ (concatMap (frv []) b1)              
frv elems _ = elems      

isVar :: Expr -> Bool
isVar (ExpVar _) = True
isVar _ = False

getId :: Expr -> String
getId (ExpVar id) = id

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

close :: F -> F
close (FUnion fs) = FUnion $ fmap close fs
close (FTable ts Unique) = FTable ts Closed
close (FTable ts Open  ) = FTable ts Closed
close f = f

open :: F -> F
open (FUnion fs) = FUnion $ fmap open fs
open (FTable ts Unique) = FTable ts Open
open f = f


reopen :: F -> F
reopen (FTable ts Closed) = FTable ts Open
reopen f = f


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
        negList = fmap not
        checkedSubtypes = negList $ fmap isSubtype productList
        
    in allT checkedSubtypes && allT (fmap (not . tag Unique) vTypes) && allT (fmap (not . tag Open) vTypes)  


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
    | otherwise = if isNothing mf then FNil else nilF  . fromJust $ mf 

fipt,fopt :: S -> F -> Int -> S
fipt s@(SP _) discF i = let projRes = fit (proj s i) discF
                        in if projRes == RVoid then SUnion [] else s 
fipt s@(SUnion ps) discF i = let fP = filter (\x -> fit (proj (SP x) i) discF /= RVoid ) ps
                             in SUnion fP

fopt s@(SP _) discF i = let projRes = fot (proj s i) discF
                        in if projRes == RVoid then SUnion [] else s 
fopt s@(SUnion ps) discF i = let fP = filter (\x -> fot (proj (SP x) i) discF /= RVoid ) ps
                             in SUnion fP


filterFun :: F -> F -> F
filterFun (FUnion fs) f = let unique = reverse . nub . reverse
                              filtered = filter (/= f) fs
                              result = unique $ fmap (`filterFun` f) filtered 
                          in if length result == 1 
                             then head result
                             else FUnion result
filterFun f1 f2 = f1
