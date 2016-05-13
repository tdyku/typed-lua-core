{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}


module Typechecker.Type where

import Data.Map                 (Map, lookup, insert, empty)
import Control.Monad.Except     (throwError)
import Control.Lens
import Control.Monad.State      (put, get)

import Types (F(..), L(..), B(..), P(..), S)
import AST (Expr(..), Stm(..), Block(..), LHVal(..), ExprList(..))
import Typechecker.Subtype ((<?))
import Typechecker.Utils


    

tBlock :: Block -> TypeState ()
tBlock (Block bs) = mapM_ tStmt bs

tStmt :: Stm -> TypeState ()
tStmt Skip = tSkip Skip
tStmt t@(StmTypedVarDecl _ _ _) = tLocal1 t
tStmt a@(StmAssign _ _) = tAssignment a


-- T-LOCAL1
tLocal1 :: Stm -> TypeState ()
tLocal1 (StmTypedVarDecl fvars exps (Block blck)) = do
    let P texps (Just e) = tExpList1 exps
        tvars = fmap snd fvars
        varTypeTuple = tupleZip texps e tvars FValue
        typingResult = fmap (\(x,y) -> x <? y) varTypeTuple
    case any (\x -> x /= True) typingResult of
        True -> throwError $ "tLocal1 error" 
        False -> do
            env <- get
            let gammaMap = env ^. gamma
                newMap = foldl insertFun gammaMap fvars
            put $ env & gamma .~ newMap
            mapM_ tStmt blck 
    where insertFun gmap (k,v) = insert k v gmap        

---- T-LOCAL2
----tLocal2 :: Stat -> TypeState ()
----tLocal2 (LocalAssign vars exps (Block blck)) = do
----    let P texps (Just e) = tExpList exps




---- T-SKIP
tSkip :: Stm -> TypeState ()
tSkip _ = return ()

-- T-EXPLIST1
tExpList1 :: ExprList -> P
tExpList1 (ExprList exps mR) = P (fmap getTypeExp exps) (Just FNil)

---- T-LHSLIST
tLHSList :: [LHVal] -> TypeState P
tLHSList vars = do
    fs <- mapM getTypeId vars
    return $ P fs (Just FValue)

-- T-ASSIGNMENT1
tAssignment :: Stm -> TypeState ()
tAssignment (StmAssign vars exps) = do
    let P texps (Just e) = tExpList1 exps
    P tvars (Just v) <- tLHSList vars
    let varTypeTuple = tupleZip texps e tvars v
    let typingResult = fmap (\(x,y) -> x <? y) varTypeTuple 
    tlog $ "Assignment: " ++ (show typingResult)
    case all id typingResult of
        True -> return ()
        False -> throwError "False in tAssignment"


getTypeExp :: Expr -> F
getTypeExp = \case
    ExpNil              -> FNil
    ExpTrue             -> FL LTrue
    ExpFalse            -> FL LFalse
    ExpInt s            -> FL (LInt s)
    ExpFloat s          -> FL (LFloat s)
    ExpString s         -> FL (LString s)
    ExpTypeCoercion f _ -> f

getTypeId :: LHVal -> TypeState F
getTypeId (IdVal id) = do
    env <- get
    case env ^. gamma ^.at id of
        Just f -> return f
        Nothing -> throwError $ "Cannot find variable" ++ id

tupleZip ls l rs r | length ls == length rs = zip ls rs
                   | length ls < length rs = zip (ls ++ repeat l) rs
                   | otherwise = zip ls (rs ++ repeat r)