{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}


module Typechecker.Type where

import Data.Map                 (Map, lookup, insert, empty)
import Control.Monad.Except     (throwError)
import Control.Lens
import Control.Monad.State      (put, get)
import Data.List                (transpose)

import Types (F(..), L(..), B(..), P(..), S(..), TType(..), T(..), E(..), R(..), specialResult)
import AST (Expr(..), Stm(..), Block(..), LHVal(..), ExprList(..), AOp(..), Appl(..), BOp(..), UnOp(..))
import Typechecker.Subtype ((<?))
import Typechecker.Utils
import Typechecker.AuxFuns (infer, fit, fot, proj, fopt, fipt)


    

tBlock :: Block -> TypeState ()
tBlock (Block bs) = mapM_ tStmt bs

tStmt :: Stm -> TypeState ()
tStmt Skip = tSkip Skip
tStmt t@(StmTypedVarDecl _ _ _) = tLocal1 t
tStmt t@(StmVarDecl _ _ _)      = tLocal2 t
tStmt a@(StmAssign _ _)         = tAssignment a
tStmt i@(StmIf _ _ _)           = tIF i

-- T-LOCAL1
tLocal1 :: Stm -> TypeState ()
tLocal1 (StmTypedVarDecl fvars exps (Block blck)) = do
    e <- tExpList exps
    expListS <- e2s e
    let tvars = fmap snd fvars
        fvarsS = SP $ P tvars (Just FValue)
    case expListS <? fvarsS of
        False -> throwError $ "tLocal1 error" 
        True  -> do
            mapM_ (\(k,v) -> insertToGamma k (TF v)) fvars
            mapM_ tStmt blck 
    where insertFun gmap (k,v) = insert k (TF v) gmap        


tLocal2 :: Stm -> TypeState ()
tLocal2 (StmVarDecl ids exprList (Block blck)) = do
    etype <- tExpList exprList
    mapM_ (registerVar etype) (zip ids [0..])
    mapM_ tStmt blck
    where registerVar :: E -> (String, Int) -> TypeState ()
          registerVar etype (id, pos) = insertToGamma id (infer etype pos)

getAppType :: Appl -> TypeState S
getAppType = error "getAppType"

-- T-LHSLIST
tLHSList :: [LHVal] -> TypeState S
tLHSList vars = do
    fs <- mapM getSimpleTypeVar vars
    return . SP $ P fs (Just FValue)
    where getSimpleTypeVar :: LHVal -> TypeState F
          getSimpleTypeVar (IdVal id) = do
            (lookupGamma id) >>= \case
                  (TF f) -> return f
                  (TFilter _ f1) -> return f1


 --T-EXPLIST 1, 1, 1
tExpList :: ExprList -> TypeState E
tExpList (ExprList exps Nothing) = E <$> (mapM getTypeExp exps) <*> (pure . Just . TF $ FNil)
tExpList (ExprList exps (Just me)) = do
    appType <- getAppType me
    tExps <- mapM getTypeExp exps
    case appType of
        SP (P fs mf) -> E <$> merge tExps fs <*> mF2mT mf
        SUnion ps -> ps2Projections tExps ps

    where merge tExps fs = do
            return $ tExps ++ (fmap TF fs)
          mF2mT maybeF = return $ fmap TF maybeF

ps2Projections :: [T] -> [P] -> TypeState E
ps2Projections tExps ps = do
    x <- tic
    insertSToPi x (SUnion ps)
    let unwrapped = fmap unwrap ps
        maxLen = maximum $ fmap length unwrapped
        projections = fmap (TProj x) [1..maxLen]
    E <$> return (tExps ++ projections) <*> (pure . Just . TF $ FNil)

  where unwrap (P fs _) = fs


tIF :: Stm -> TypeState ()
tIF (StmIf cond tBlk eBlk) =
  case cond of 
    ExpVar id -> do
      idType <- lookupGamma id
      case idType of
        TF f -> do 
          let ft = fot f FNil
              fe = fit f FNil
          if ft /= RVoid
          then do newGammaScope
                  insertToGamma id (TFilter f (specialResult ft))
                  tBlock tBlk
                  popGammaScope
          else return ()
          if fe /= RVoid
          then do newGammaScope
                  insertToGamma id (TFilter f (specialResult fe))
                  tBlock eBlk
                  popGammaScope
          else return ()
                  
        TProj x i -> do
          sX <- lookupPI x
          if (fit (proj sX i) FNil) == RVoid
          then (do
            newPiScope
            let sT = fopt sX FNil i
            insertSToPi x sT
            tBlock tBlk 
            popPiScope)
          else if (fot (proj sX i) FNil) == RVoid
               then (do
                newPiScope
                let sE = fipt sX FNil i
                insertSToPi x sE
                tBlock eBlk
                popPiScope)
               else (do
                let sT = fopt sX FNil i
                    sE = fipt sX FNil i
                newPiScope
                insertSToPi x sT
                tBlock tBlk
                popPiScope
                insertSToPi x sE
                tBlock eBlk
                popPiScope)
        _ -> normalCase cond tBlk eBlk
    ExpABinOp Equals (ExpOneResult (FunAppl (ExpVar "type") (ExprList [ExpVar "id"] Nothing))) (ExpString "string") -> do
      idType <- lookupGamma "id"
      case idType of
        TFilter f1 f2 -> do
          let rT = fit f2 (FB BString)
              rE = fot f2 (FB BString)
          if rT == RVoid
          then (do
            newGammaScope
            let (RF fE) = rE
            insertToGamma "id" (TFilter f1 fE)
            tBlock eBlk
            popGammaScope
            )
          else if rE == RVoid
               then (do
                newGammaScope
                let (RF fT) = rT
                insertToGamma "id" (TFilter f1 fT)
                tBlock tBlk
                popGammaScope
                )
               else (do
                let (RF fT) = rT
                let (RF fE) = rE
                newGammaScope
                insertToGamma "id" (TFilter f1 fT)
                tBlock tBlk
                popGammaScope
                newGammaScope
                insertToGamma "id" (TFilter f1 fE)
                tBlock eBlk
                popGammaScope
                )
        _ -> normalCase cond tBlk eBlk     

    _ -> normalCase cond tBlk eBlk
  where normalCase cond (Block tBlk) (Block eBlk) = do
          getTypeExp cond
          mapM_ tStmt tBlk
          mapM_ tStmt eBlk              




-- T-SKIP
tSkip :: Stm -> TypeState ()
tSkip _ = return ()

-- T-ASSIGNMENT1
tAssignment :: Stm -> TypeState ()
tAssignment (StmAssign vars exps) = do
    texps <- tExpList exps
    s1 <- e2s texps
    s2 <- tLHSList vars
    tlog s1
    tlog s2
    if s1 <? s2
    then tlog $ "Assignment: " ++ show s1 ++ " " ++ show s2
    else throwError "False in tAssignment"


getTypeExp :: Expr -> TypeState T
getTypeExp = \case
    ExpNil                     -> return . TF $ FNil
    ExpTrue                    -> return . TF $ FL LTrue
    ExpFalse                   -> return . TF $ FL LFalse
    ExpInt s                   -> return . TF . FL $ LInt s
    ExpFloat s                 -> return . TF . FL $ LFloat s
    ExpString s                -> return . TF . FL $ LString s
    ExpTypeCoercion f _        -> return . TF $ f
    ExpVar var                 -> lookupGamma var 
    e@(ExpABinOp Add _ _)      -> TF <$> tArith e
    e@(ExpABinOp Concat _ _)   -> TF <$> tConcat e
    e@(ExpABinOp Equals _ _)   -> TF <$> tEqual e
    e@(ExpABinOp LessThan _ _) -> TF <$> tOrder e
    e@(ExpBBinOp Amp _ _)      -> TF <$> tBitWise e
    e@(ExpBBinOp And _ _)      -> TF <$> tAnd e
    e@(ExpBBinOp Or _ _)       -> TF <$> tOr e
    e@(ExpUnaryOp Not _)       -> TF <$> tNot e
    e@(ExpUnaryOp Hash _)      -> TF <$> tLen e    





tArith :: Expr -> TypeState F
tArith (ExpABinOp Add e1 e2) = do
    TF f1 <- getTypeExp e1
    TF f2 <- getTypeExp e2
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
    TF f1 <- getTypeExp e1
    TF f2 <- getTypeExp e2
    if f1 <? (FB BString) && f2 <? (FB BString)
    then return (FB BString)
    else if f1 == FAny && f2 == FAny
         then return FAny
         else throwError "tConcat cannot typecheck"

tEqual :: Expr -> TypeState F
tEqual (ExpABinOp Equals e1 e2) = return (FB BBoolean)

tOrder :: Expr -> TypeState F
tOrder (ExpABinOp LessThan e1 e2) = do
    TF f1 <- getTypeExp e1
    TF f2 <- getTypeExp e2
    if f1 <? (FB BNumber) && f2 <? (FB BNumber) 
    then return (FB BBoolean)
    else if f1 <? (FB BString) && f2 <? (FB BString)
         then return (FB BString)
         else if f1 == FAny || f2 == FAny
              then return FAny
              else throwError "tOrder cannot typecheck"


tBitWise :: Expr -> TypeState F
tBitWise (ExpBBinOp Amp e1 e2) = do
    TF f1 <- getTypeExp e1
    TF f2 <- getTypeExp e2
    if f1 <? (FB BInt) && f2 <? (FB BInt)
    then return (FB BInt)
    else if f1 == FAny || f2 == FAny
         then return FAny
         else throwError "tBitWise cannot typecheck"


tAnd :: Expr -> TypeState F
tAnd (ExpBBinOp And e1 e2) = do
    TF f1 <- getTypeExp e1
    TF f2 <- getTypeExp e2
    if f1 == FNil || f1 == (FL LFalse) || f1 == FUnion [FNil, FL LFalse]
    then return f1
    else if not (FNil <? f1) && not ((FL LFalse) <? f1)
         then return f2
         else return $ FUnion [f1, f2]

tOr :: Expr -> TypeState F
tOr (ExpBBinOp Or e1 e2) = do
    TF f1 <- getTypeExp e1
    TF f2 <- getTypeExp e2  
    if not (FNil <? f1) && not ((FL LFalse)  <? f2)
    then return f1
    else if f1 == FNil || f1 == (FL LFalse) || f1 == FUnion [FNil, FL LFalse]
         then return f2
         else throwError "tOr unimplemented tOr5"

tNot :: Expr -> TypeState F
tNot (ExpUnaryOp Not e1) = do
    TF f <- getTypeExp e1
    if f == FNil || f == (FL LFalse) || f == FUnion [FNil, FL LFalse]
    then return $ FL LTrue
    else if not (FNil <? f) && not ((FL LFalse) <? f)
         then return $ FL LFalse
         else return $ FB BBoolean

tLen :: Expr -> TypeState F
tLen (ExpUnaryOp Hash e1) = do
    TF f <- getTypeExp e1
    if f <? (FB BString) || f <? (FTable [] Closed)
    then return $ FB BInt
    else if f == FAny
         then return FAny 
         else throwError "tLen cannot typecheck"