module Typechecker.AuxFuns where

import Control.Monad.State      (State, StateT, liftIO, get, put, evalStateT)
import Control.Monad.Except     (ExceptT, throwError, runExceptT)
import Control.Lens
import Types                    (F(..), L(..), B(..), T(..), P(..), S(..), E(..))
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