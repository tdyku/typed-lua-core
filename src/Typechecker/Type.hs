{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}


module Typechecker.Type where

import Data.Map                 (Map, lookup, insert, empty)
import Control.Monad.Except     (throwError)
import Control.Lens
import Control.Monad.State      (put, get)
import Data.List                (transpose)

import Types (F(..), L(..), B(..), P(..), S(..), TType(..), T(..), E(..), R(..), specialResult, V(..))
import AST (Expr(..), Stm(..), Block(..), LHVal(..), ExprList(..), AOp(..), ParamList(..), Appl(..), BOp(..), UnOp(..))
import Typechecker.Subtype ((<?))
import Typechecker.Utils
import Typechecker.AuxFuns (infer, fit, fot, proj, fopt, fipt, wf)


    

tBlock :: Block -> TypeState ()
tBlock (Block bs) = mapM_ tStmt bs

tStmt :: Stm -> TypeState ()
tStmt Skip = tSkip Skip
tStmt t@(StmTypedVarDecl _ _ _) = tLocal1 t
tStmt t@(StmVarDecl _ _ _)      = tLocal2 t
tStmt a@(StmAssign _ _)         = tAssignment a
tStmt i@(StmIf _ _ _)           = tIF i
tStmt w@(StmWhile _ _)          = tWhile w
tStmt r@(StmReturn _)           = tReturn r

-- T-LOCAL1
tLocal1 :: Stm -> TypeState ()
tLocal1 (StmTypedVarDecl fvars exps (Block blck)) = do
    e <- tExpList exps
    expListS <- e2s e
    let tvars = fmap snd fvars
        fvarsS = SP $ P tvars (Just FValue)
    case expListS <? fvarsS of
        False -> throwError $ "tLocal1 error:\n" ++ show expListS ++ "\n" ++ show fvars 
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

tApply :: Appl -> TypeState S
tApply (FunAppl e eList) = do
    funType <- getTypeExp e
    tArgs <- tExpList eList
    case funType of
      TF (FFunction s1 s2) -> do
        let e1 = sp2e s1
        if tArgs <? e1 
        then return s2
        else throwError "Given args does not match function."

      TF FAny -> return . SP . (P []) . Just $ FAny  
      _ -> throwError "Expression is not a function in tApply."


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
    appType <- tApply me
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
        projections = fmap (TProj x) [0..maxLen-1]
    E <$> return (tExps ++ projections) <*> (pure . Just . TF $ FNil)

  where unwrap (P fs _) = fs


readExp :: T -> TypeState T
readExp (TF f) = return $ TF f
readExp (TFilter _ f2) = return $ TF f2
readExp (TProj x1 i1) = do
  sX <- lookupPI x1
  return . TF $ proj sX i1


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

tReturn :: Stm -> TypeState ()
tReturn (StmReturn explist) = return () -- we typecheck returns in function body 

tWhile :: Stm -> TypeState ()
tWhile (StmWhile e blk) = do
  getTypeExp e
  tBlock blk


-- T-SKIP
tSkip :: Stm -> TypeState ()
tSkip _ = return ()

-- T-ASSIGNMENT1
tAssignment :: Stm -> TypeState ()
tAssignment (StmAssign vars exps) = do
    texps <- tExpList exps
    s1 <- e2s texps
    s2 <- tLHSList vars
    if s1 <? s2
    then tlog $ "Assignment: " ++ show s1 ++ " " ++ show s2
    else throwError $ "False in tAssignment" ++ show s1 ++ show s2


getTypeExp :: Expr -> TypeState T
getTypeExp = \case
    ExpNil                       -> return . TF $ FNil
    ExpTrue                      -> return . TF $ FL LTrue
    ExpFalse                     -> return . TF $ FL LFalse
    ExpInt s                     -> return . TF . FL $ LInt s
    ExpFloat s                   -> return . TF . FL $ LFloat s
    ExpString s                  -> return . TF . FL $ LString s
    ExpTypeCoercion f _          -> return . TF $ f
    ExpVar var                   -> lookupGamma var 
    e@(ExpABinOp Add _ _)        -> TF <$> tArith e
    e@(ExpABinOp Div _ _)        -> TF <$> tDiv e
    e@(ExpABinOp Mod _ _)        -> TF <$> tMod e    
    e@(ExpABinOp Concat _ _)     -> TF <$> tConcat e
    e@(ExpABinOp Equals _ _)     -> TF <$> tEqual e
    e@(ExpABinOp IntDiv _ _)     -> TF <$> tIntDiv e
    e@(ExpABinOp LessThan _ _)   -> TF <$> tOrder e
    e@(ExpBBinOp Amp _ _)        -> TF <$> tBitWise e
    e@(ExpBBinOp And _ _)        -> TF <$> tAnd e
    e@(ExpBBinOp Or _ _)         -> TF <$> tOr e
    e@(ExpUnaryOp Not _)         -> TF <$> tNot e
    e@(ExpUnaryOp Hash _)        -> TF <$> tLen e
    f@(ExpFunDecl _ _ _)         -> TF <$> tFun f
    t@(ExpTableConstructor es a) -> TF <$> tConstr es a


tConstr :: [(Expr, Expr)] -> Maybe Appl -> TypeState F
tConstr es Nothing = do
  keyTypes <- mapM (getTypeExp . fst) es
  mapTypes <- mapM (getTypeExp . snd) es
  fTypes <- mapM inferF keyTypes
  vTypes <- mapM inferV mapTypes
  let tableType = FTable (zip fTypes vTypes) Unique
  if wf tableType then return tableType else throwError "Table is not well formed"

  where inferF :: T -> TypeState F
        inferF (TF f) = return f
        inferF _ = throwError "tConstr, table fields should be F"
        inferV :: T -> TypeState V
        inferV (TF f) = return . VF $ f
        inferV _ = throwError "tConstr, table fields should be F"
        



tFun :: Expr -> TypeState F
tFun (ExpFunDecl (ParamList tIds mf) s blk@(Block b)) = do
    newScopes
    let argType = SP $ P (fmap snd tIds) mf
    mapM_ (\(k,v) -> insertToGamma k (TF v)) tIds
    tBlock blk

    popScopes
    return $ FFunction argType s


tDiv :: Expr -> TypeState F
tDiv (ExpABinOp Div e1 e2) = do
    TF f1 <- (getTypeExp e1 >>= readExp)
    TF f2 <- (getTypeExp e2 >>= readExp)
    if f1 <? (FB BInt) && f2 <? (FB BInt)
    then return (FB BInt)
    else if (f1 <? (FB BInt) && f2 <? (FB BNumber)) || (f2 <? (FB BInt) && f1 <? (FB BNumber))
         then return (FB BNumber)
         else if f1 <? (FB BNumber) && f2 <? (FB BNumber)
              then return (FB BNumber)
              else if f1 == FAny || f2 == FAny 
              then return FAny
              else throwError "tDiv cannot typecheck"


tIntDiv :: Expr -> TypeState F
tIntDiv (ExpABinOp IntDiv e1 e2) = do
      TF f1 <- getTypeExp e1 >>= readExp
      TF f2 <- getTypeExp e2 >>= readExp
      if f1 <? FB BInt && f2 <? FB BInt
      then return (FB BInt)
      else if (f1 <? (FB BInt) && f2 <? (FB BNumber)) || (f2 <? (FB BInt) && f1 <? (FB BNumber))
           then return (FB BInt)
           else if f1 <? (FB BNumber) && f2 <? (FB BNumber)
                then return (FB BInt)
                else if f1 == FAny || f2 == FAny 
                then return FAny
                else throwError "tIntDiv cannot typecheck"


tMod :: Expr -> TypeState F
tMod (ExpABinOp Mod e1 e2) = do
    TF f1 <- getTypeExp e1 >>= readExp
    TF f2 <- getTypeExp e2 >>= readExp
    if f1 <? FB BInt && f2 <? FB BInt
    then return (FB BInt)
    else if (f1 <? FB BInt && f2 <? FB BNumber) || (f2 <? FB BInt && f1 <? FB BNumber)
         then return (FB BNumber)
         else if f1 <? FB BNumber && f2 <? FB BNumber
              then return (FB BNumber)
              else if f1 == FAny || f2 == FAny 
              then return FAny
              else throwError "tMod cannot typecheck"



tArith :: Expr -> TypeState F
tArith (ExpABinOp Add e1 e2) = do
    TF f1 <- getTypeExp e1 >>= readExp
    TF f2 <- getTypeExp e2 >>= readExp
    if f1 <? FB BInt && f2 <? FB BInt
    then return (FB BInt)
    else if (f1 <? FB BInt && f2 <? FB BNumber) || (f2 <? FB BInt && f1 <? FB BNumber)
         then return (FB BNumber)
         else if f1 <? FB BNumber && f2 <? FB BNumber
              then return (FB BNumber)
              else if f1 == FAny || f2 == FAny 
              then return FAny
              else throwError "tArith cannot typecheck"



tConcat :: Expr -> TypeState F
tConcat (ExpABinOp Concat e1 e2) = do
    TF f1 <- getTypeExp e1 >>= readExp
    TF f2 <- getTypeExp e2 >>= readExp
    if f1 <? FB BString && f2 <? FB BString
    then return (FB BString)
    else if f1 == FAny && f2 == FAny
         then return FAny
         else throwError "tConcat cannot typecheck"

tEqual :: Expr -> TypeState F
tEqual (ExpABinOp Equals e1 e2) = return (FB BBoolean)

tOrder :: Expr -> TypeState F
tOrder (ExpABinOp LessThan e1 e2) = do
    TF f1 <- getTypeExp e1 >>= readExp
    TF f2 <- getTypeExp e2 >>= readExp
    if f1 <? FB BNumber && f2 <? FB BNumber
    then return (FB BBoolean)
    else if f1 <? FB BString && f2 <? FB BString
         then return (FB BString)
         else if f1 == FAny || f2 == FAny
              then return FAny
              else throwError "tOrder cannot typecheck"


tBitWise :: Expr -> TypeState F
tBitWise (ExpBBinOp Amp e1 e2) = do
    TF f1 <- getTypeExp e1 >>= readExp
    TF f2 <- getTypeExp e2 >>= readExp
    if f1 <? FB BInt && f2 <? FB BInt
    then return (FB BInt)
    else if f1 == FAny || f2 == FAny
         then return FAny
         else throwError "tBitWise cannot typecheck"


tAnd :: Expr -> TypeState F
tAnd (ExpBBinOp And e1 e2) = do
    TF f1 <- getTypeExp e1 >>= readExp
    TF f2 <- getTypeExp e2 >>= readExp
    if f1 == FNil || f1 == FL LFalse || f1 == FUnion [FNil, FL LFalse]
    then return f1
    else if not (FNil <? f1) && not (FL LFalse <? f1)
         then return f2
         else return $ FUnion [f1, f2]

tOr :: Expr -> TypeState F
tOr (ExpBBinOp Or e1 e2) = do
    TF f1 <- getTypeExp e1 >>= readExp
    TF f2 <- getTypeExp e2 >>= readExp
    if not (FNil <? f1) && not (FL LFalse  <? f2)
    then return f1
    else if f1 == FNil || f1 == FL LFalse || f1 == FUnion [FNil, FL LFalse]
         then return f2
         else throwError "tOr unimplemented tOr5"

tNot :: Expr -> TypeState F
tNot (ExpUnaryOp Not e1) = do
    TF f <- getTypeExp e1 >>= readExp
    if f == FNil || f == FL LFalse || f == FUnion [FNil, FL LFalse]
    then return $ FL LTrue
    else if not (FNil <? f) && not (FL LFalse <? f)
         then return $ FL LFalse
         else return $ FB BBoolean

tLen :: Expr -> TypeState F
tLen (ExpUnaryOp Hash e1) = do
    TF f <- getTypeExp e1 >>= readExp
    if f <? FB BString || f <? FTable [] Closed
    then return $ FB BInt
    else if f == FAny
         then return FAny 
         else throwError "tLen cannot typecheck"