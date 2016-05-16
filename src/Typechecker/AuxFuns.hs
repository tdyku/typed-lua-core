module Typechecker.AuxFuns where

import Control.Monad.State      (State, StateT, liftIO, get, put, evalStateT)
import Control.Monad.Except     (ExceptT, throwError, runExceptT)
import Control.Lens
import Types                    (F(..), L(..), B(..), T(..), R(..), P(..), S(..), E(..))
import Data.Map
import Data.Map                 (lookup)
import Prelude                  hiding (pi, lookup)
import Typechecker.Subtype      ((<?))


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
