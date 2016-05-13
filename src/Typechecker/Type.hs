{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}


module Typechecker.Type where

import Data.Map                 (Map, lookup, insert, empty)
import Control.Monad.Except     (throwError)
import Control.Lens
import Control.Monad.State      (put, get)

import Types (F(..), L(..), B(..), P(..), S, TType(..))
import AST (Expr(..), Stm(..), Block(..), LHVal(..), ExprList(..), AOp(..), BOp(..), UnOp(..))
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
    P texps (Just e) <- tExpList1 exps
    let tvars = fmap snd fvars
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
tExpList1 :: ExprList -> TypeState P
tExpList1 (ExprList exps mR) = P <$> (mapM getTypeExp exps) <*> pure (Just FNil)

---- T-LHSLIST
tLHSList :: [LHVal] -> TypeState P
tLHSList vars = do
    fs <- mapM getTypeId vars
    return $ P fs (Just FValue)

-- T-ASSIGNMENT1
tAssignment :: Stm -> TypeState ()
tAssignment (StmAssign vars exps) = do
    P texps (Just e) <- tExpList1 exps
    P tvars (Just v) <- tLHSList vars
    let varTypeTuple = tupleZip texps e tvars v
    let typingResult = fmap (\(x,y) -> x <? y) varTypeTuple 
    tlog $ "Assignment: " ++ (show typingResult)
    case all id typingResult of
        True -> return ()
        False -> throwError "False in tAssignment"


getTypeExp :: Expr -> TypeState F
getTypeExp = \case
    ExpNil                     -> return $ FNil
    ExpTrue                    -> return $ FL LTrue
    ExpFalse                   -> return $ FL LFalse
    ExpInt s                   -> return $ FL (LInt s)
    ExpFloat s                 -> return $ FL (LFloat s)
    ExpString s                -> return $ FL (LString s)
    ExpTypeCoercion f _        -> return $ f
    e@(ExpABinOp Add _ _)      -> tArith e
    e@(ExpABinOp Concat _ _)   -> tConcat e
    e@(ExpABinOp Equals _ _)   -> tEqual e
    e@(ExpABinOp LessThan _ _) -> tOrder e
    e@(ExpBBinOp Amp _ _)      -> tBitWise e
    e@(ExpBBinOp And _ _)      -> tAnd e
    e@(ExpBBinOp Or _ _)       -> tOr e
    e@(ExpUnaryOp Not _)       -> tNot e
    e@(ExpUnaryOp Hash _)      -> tLen e    



getTypeId :: LHVal -> TypeState F
getTypeId (IdVal id) = do
    env <- get
    case env ^. gamma ^.at id of
        Just f -> return f
        Nothing -> throwError $ "Cannot find variable" ++ id

tupleZip ls l rs r | length ls == length rs = zip ls rs
                   | length ls < length rs = zip (ls ++ repeat l) rs
                   | otherwise = zip ls (rs ++ repeat r)


-- TODO: components should have type F
tArith :: Expr -> TypeState F
tArith (ExpABinOp Add e1 e2) = do
    f1 <- getTypeExp e1
    f2 <- getTypeExp e2
    if f1 <? (FB BInt) && f2 <? (FB BInt)
    then return (FB BInt)
    else if (f1 <? (FB BInt) && f2 <? (FB BNumber)) || (f2 <? (FB BInt) && f1 <? (FB BNumber))
         then return (FB BNumber)
         else if f1 <? (FB BNumber) && f2 <? (FB BNumber)
              then return (FB BNumber)
              else if f1 == FAny || f2 == FAny 
              then return FAny
              else throwError "tArith cannot typecheck"



tConcat :: Expr -> TypeState F
tConcat (ExpABinOp Concat e1 e2) = do
    f1 <- getTypeExp e1
    f2 <- getTypeExp e2
    if f1 <? (FB BString) && f2 <? (FB BString)
    then return (FB BString)
    else if f1 == FAny && f2 == FAny
         then return FAny
         else throwError "tConcat cannot typecheck"

tEqual :: Expr -> TypeState F
tEqual (ExpABinOp Equals e1 e2) = return (FB BBoolean)

tOrder :: Expr -> TypeState F
tOrder (ExpABinOp LessThan e1 e2) = do
    f1 <- getTypeExp e1
    f2 <- getTypeExp e2
    if f1 <? (FB BNumber) && f2 <? (FB BNumber) 
    then return (FB BBoolean)
    else if f1 <? (FB BString) && f2 <? (FB BString)
         then return (FB BString)
         else if f1 == FAny || f2 == FAny
              then return FAny
              else throwError "tOrder cannot typecheck"


tBitWise :: Expr -> TypeState F
tBitWise (ExpBBinOp Amp e1 e2) = do
    f1 <- getTypeExp e1
    f2 <- getTypeExp e2
    if f1 <? (FB BInt) && f2 <? (FB BInt)
    then return (FB BInt)
    else if f1 == FAny || f2 == FAny
         then return FAny
         else throwError "tBitWise cannot typecheck"


tAnd :: Expr -> TypeState F
tAnd (ExpBBinOp And e1 e2) = do
    f1 <- getTypeExp e1
    f2 <- getTypeExp e2
    if f1 == FNil || f1 == (FL LFalse) || f1 == FUnion [FNil, FL LFalse]
    then return f1
    else if not (FNil <? f1) && not ((FL LFalse) <? f1)
         then return f2
         else return $ FUnion [f1, f2]

tOr :: Expr -> TypeState F
tOr (ExpBBinOp Or e1 e2) = do
    f1 <- getTypeExp e1
    f2 <- getTypeExp e2  
    if not (FNil <? f1) && not ((FL LFalse)  <? f2)
    then return f1
    else if f1 == FNil || f1 == (FL LFalse) || f1 == FUnion [FNil, FL LFalse]
         then return f2
         else throwError "tOr unimplemented tOr5"

tNot :: Expr -> TypeState F
tNot (ExpUnaryOp Not e1) = do
    f <- getTypeExp e1
    if f == FNil || f == (FL LFalse) || f == FUnion [FNil, FL LFalse]
    then return $ FL LTrue
    else if not (FNil <? f) && not ((FL LFalse) <? f)
         then return $ FL LFalse
         else return $ FB BBoolean

tLen :: Expr -> TypeState F
tLen (ExpUnaryOp Hash e1) = do
    f <- getTypeExp e1
    if f <? (FB BString) || f <? (FTable [] Closed)
    then return $ FB BInt
    else if f == FAny
         then return FAny 
         else throwError "tLen cannot typecheck"