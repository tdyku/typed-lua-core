{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}


module Typechecker.Type where

import Data.Map                 (Map, lookup, insert, empty, mapMaybeWithKey)
import Control.Monad.Except     (throwError)
import Control.Lens
import Control.Monad.State      (put, get)
import Data.List                (transpose)
import Prelude                  hiding (pi)

import Types (F(..), L(..), B(..), P(..), S(..), TType(..), T(..), E(..), R(..), specialResult, V(..))
import AST (Expr(..), Stm(..), Block(..), LHVal(..), ExprList(..), AOp(..), ParamList(..), Appl(..), BOp(..), UnOp(..))
import Typechecker.Subtype ((<?))
import Typechecker.Utils
import Typechecker.AuxFuns
import Text.Show.Pretty (ppShow)

    

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
tStmt m@(StmMthdDecl _ _ _ _ _) = tMethod m

tMethod :: Stm -> TypeState ()
tMethod m@(StmMthdDecl tabId _ _ _ _) = do
  f <- tSimpleLookup tabId
  case tableType f of
    Unique -> tMethodUnique m
    Open -> error "Open!!!!!!!!!!!!"
    Closed -> throwError "Methods can be defined only in Open and Unique tables"


tMethodUnique :: Stm -> TypeState ()
tMethodUnique m@(StmMthdDecl tabId funId (ParamList tIds _) retType stms) = do
  t@(FTable tls ttp) <- tSimpleLookup tabId
  if all (==False) $ fmap ((FL (LString tabId)) <?) (fst <$> tls)
  then do 
    let filteredFav nms = fmap getIdVal $ filter isIdVal nms
    closeAll
    newGammaScope
    insertToGamma "self" (TF t)
    mapM_ (\(k,v) -> insertToGamma k (TF v)) tIds
    tBlock stms
    popGammaScope
    let fun = FFunction (SP $ P (FSelf : (snd <$> tIds)) (Just FNil)) retType
    insertToGamma tabId (TF $ FTable (tls ++ [(FL $ LString tabId,VConst fun)]) ttp)
    openSet (frv [] m)
    closeSet (filteredFav $ fav [] m)
  
  else throwError $ "Method " ++ funId ++ " overrides key!\n" 





-- T-LOCAL1
tLocal1 :: Stm -> TypeState ()
tLocal1 (StmTypedVarDecl fvars exps (Block blck)) = do
    e <- tExpList exps
    expListS <- e2s e
    let tvars = fmap snd fvars
        fvarsS = SP $ P tvars (Just FValue)
    case expListS <? fvarsS of
        False -> throwError $ "tLocal1 error:\n" ++ show fvarsS ++ "\n" ++ show expListS
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


getSimpleTypeVar :: LHVal -> TypeState F
getSimpleTypeVar (IdVal id) = do
  (lookupGamma id) >>= \case
        (TF f) -> return f
        (TFilter _ f1) -> return f1      

getSimpleTypeVar (TableVal id expr) = do
  table <- getSimpleTypeVar (IdVal id)
  TF index <- getTypeExp expr
  findValue table index
  where findValue :: F -> F -> TypeState F
        findValue (FTable ts _) f = scanPairs ts f
        findValue FAny _ = return FAny
        findValue t _ = throwError $ "Left side of accessor should be table."
        
        scanPairs :: [(F, V)] -> F -> TypeState F
        scanPairs [] _ = throwError $ "No such field in table!"
        scanPairs (t:ts) f = if f <? (fst t) && not  (isConst $ snd t) then return . rconst . snd $ t else scanPairs ts f

getSimpleTypeVar (TypeCoercionVal id expr v) = do
  table <- getSimpleTypeVar (IdVal id)
  TF fNew <- getTypeExp expr
  let vNew = vt fNew v
  consistent <- checkConsistency table fNew
  if consistent then updateTab id table fNew vNew >> (return . rconst $ vNew)
    else throwError "Refinement violate's consistency."

  where checkConsistency :: F -> F -> TypeState Bool
        checkConsistency (FTable tt1 tp) f = case tp of 
          Closed -> throwError "Coercion works only with Open and Unique tables, not closed"
          Fixed  -> throwError "Coercion works only with Open and Unique tables, not fixed"
          _ -> return $ allT $ fmap (not . (f <?) . fst) tt1
        updateTab id (FTable ts tp) f v = insertToGamma id (TF $ FTable (ts ++ [(f,v)]) tp)
   

 --T-EXPLIST
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


readExp :: String -> T -> TypeState T
readExp nm (TF f) = TF <$>  processF nm f
  where processF :: String -> F -> TypeState F
        processF var t@(FTable ts Unique) = insertToGamma var (TF . open $ t) >> (return . close $ t) 
        --processF var t@(FTable ts *?*) = insertToGamma var (TF . fix $ t) >> (return . fix $ t) 
        processF _ f = return f

readExp _ (TFilter _ f2) = return $ TF f2
readExp _ (TProj x1 i1) = do
  sX <- lookupPI x1
  return . TF $ proj sX i1


tSimpleLookup :: String -> TypeState F
tSimpleLookup id = do
  TF f <- lookupGamma id
  return f


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
tWhile w@(StmWhile e@(ExpVar id) blk@(Block stms)) = do
 TF f <- getTypeExp e
 insertToGamma id (TFilter f (filterFun f FNil))
 closeAll
 tBlock blk
 let filteredFav nms = fmap getIdVal $ filter isIdVal nms
 insertToGamma id (TF f)
 openSet (frv [] w)
 closeSet (filteredFav $ fav [] w)


tWhile w@(StmWhile e blk@(Block stms)) = do
  getTypeExp e
  gamma <- getGamma 
  let filteredFav nms = fmap getIdVal $ filter isIdVal nms
  closeAll
  tBlock blk
  closeSet (filteredFav $ fav [] w)
  openSet (frv [] w) 
  


isIdVal (IdVal _) = True
isIdVal _ = False

getIdVal (IdVal id) = id

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
    then return ()
    else throwError $ "False in tAssignment" ++ show s1 ++ show s2


--tLocal2 :: Stm -> TypeState ()
--tLocal2 (StmVarDecl ids exprList (Block blck)) = do
--    etype <- tExpList exprList
--    mapM_ (registerVar etype) (zip ids [0..])
--    mapM_ tStmt blck
--    where registerVar :: E -> (String, Int) -> TypeState ()
--          registerVar etype (id, pos) = insertToGamma id (infer etype pos)

getTypeExp :: Expr -> TypeState T
getTypeExp = \case
    ExpNil                       -> return . TF $ FNil
    ExpTrue                      -> return . TF $ FL LTrue
    ExpFalse                     -> return . TF $ FL LFalse
    ExpInt s                     -> return . TF . FL $ LInt s
    ExpFloat s                   -> return . TF . FL $ LFloat s
    ExpString s                  -> return . TF . FL $ LString s
    c@(ExpTypeCoercion _ _)      -> TF <$> tCoercion c
    v@(ExpVar var)               -> tLookUpId v 
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
    a@(ExpTableAccess _ _)       -> TF <$> tIndexRead a


tCoercion :: Expr -> TypeState F
tCoercion (ExpTypeCoercion f id) = do
  TF idExp <- getTypeExp (ExpVar id)
  if idExp <? f 
    then if tag Closed f
           then insertToGamma id (TF . reopen $ f) >> (return f)
           else if tag Fixed f
                  then insertToGamma id (TF f) >> (return f)
                  else throwError $ "Error in coercion: " ++ show id ++ " is neither closed nor fixed"
    else throwError $ "Error in coercion: " ++ show id ++ " is not subtype of: " ++ ppShow f


tLookUpId :: Expr -> TypeState T
tLookUpId (ExpVar var) = do
    expr <- lookupGamma var
    readExp var expr


tIndexRead :: Expr -> TypeState F
tIndexRead (ExpTableAccess e1 e2) = do
    TF table <- getTypeExp e1
    TF index <- getTypeExp e2
    findValue table index

  where findValue :: F -> F -> TypeState F
        findValue (FTable ts _) f = scanPairs ts f
        findValue FAny _ = return FAny
        findValue t _ = throwError $ "Left side of accessor should be table."
        
        scanPairs :: [(F, V)] -> F -> TypeState F
        scanPairs [] _ = throwError $ "No such field in table!"
        scanPairs (t:ts) f = if f <? (fst t) then return . rconst . snd $ t else scanPairs ts f



tConstr :: [(Expr, Expr)] -> Maybe Appl -> TypeState F
tConstr es mApp = do
  keyTypes <- mapM (getTypeExp . fst) es
  mapTypes <- mapM (getTypeExp . snd) es
  fTypes <- mapM inferF keyTypes
  vTypes <- mapM inferV mapTypes
  tableType <- case mApp of
    Nothing -> return $ FTable (zip fTypes vTypes) Unique
    Just app -> do
      tCall <- tApply app
      let (exps, vexp) = s2f tCall
          varArg = VF $ vexp
      return $ FTable (zip fTypes vTypes ++ zip (FL . LInt <$> [1..]) (VF <$> exps) ++ [(FB BInt, varArg)]) Unique
  -- TODO: well-formed checking 
  if {-wf tableType-} True then return tableType else throwError "Table is not well formed"

  where inferF :: T -> TypeState F
        inferF (TF f) = return f
        inferF _ = throwError "tConstr, table fields should be F"
        inferV :: T -> TypeState V
        inferV (TF f) = return . VF $ f
        inferV _ = throwError "tConstr, table fields should be F"
        



tFun :: Expr -> TypeState F
tFun (ExpFunDecl (ParamList tIds mf) s blk@(Block b)) = do
    newScopes
    let filteredFav nms = fmap getIdVal $ filter isIdVal nms
    let argType = SP $ P (fmap snd tIds) mf
    mapM_ (\(k,v) -> insertToGamma k (TF v)) tIds
    closeAll
    tBlock blk
    openSet (concat $ fmap (frv []) b)
    closeSet (filteredFav . concat $ fmap (fav []) b)
    popScopes
    return $ FFunction argType s


tDiv :: Expr -> TypeState F
tDiv (ExpABinOp Div e1 e2) = do
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
              else throwError "tDiv cannot typecheck"


tIntDiv :: Expr -> TypeState F
tIntDiv (ExpABinOp IntDiv e1 e2) = do
      TF f1 <- getTypeExp e1
      TF f2 <- getTypeExp e2
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
    TF f1 <- getTypeExp e1
    TF f2 <- getTypeExp e2
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
    TF f1 <- getTypeExp e1
    TF f2 <- getTypeExp e2
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
    TF f1 <- getTypeExp e1
    TF f2 <- getTypeExp e2
    if f1 <? FB BString && f2 <? FB BString
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
    if f1 <? FB BNumber && f2 <? FB BNumber
    then return (FB BBoolean)
    else if f1 <? FB BString && f2 <? FB BString
         then return (FB BString)
         else if f1 == FAny || f2 == FAny
              then return FAny
              else throwError "tOrder cannot typecheck"


tBitWise :: Expr -> TypeState F
tBitWise (ExpBBinOp Amp e1 e2) = do
    TF f1 <- getTypeExp e1
    TF f2 <- getTypeExp e2
    if f1 <? FB BInt && f2 <? FB BInt
    then return (FB BInt)
    else if f1 == FAny || f2 == FAny
         then return FAny
         else throwError "tBitWise cannot typecheck"


tAnd :: Expr -> TypeState F
tAnd (ExpBBinOp And e1 e2) = do
    TF f1 <- getTypeExp e1
    TF f2 <- getTypeExp e2
    if f1 == FNil || f1 == FL LFalse || f1 == FUnion [FNil, FL LFalse]
    then return f1
    else if not (FNil <? f1) && not (FL LFalse <? f1)
         then return f2
         else return $ FUnion [f1, f2]

tOr :: Expr -> TypeState F
tOr (ExpBBinOp Or e1 e2) = do
    TF f1 <- getTypeExp e1
    TF f2 <- getTypeExp e2
    if not (FNil <? f1) && not (FL LFalse  <? f2)
    then return f1
    else if f1 == FNil || f1 == FL LFalse || f1 == FUnion [FNil, FL LFalse]
         then return f2
         else throwError "tOr unimplemented tOr5"

tNot :: Expr -> TypeState F
tNot (ExpUnaryOp Not e1) = do
    TF f <- getTypeExp e1
    if f == FNil || f == FL LFalse || f == FUnion [FNil, FL LFalse]
    then return $ FL LTrue
    else if not (FNil <? f) && not (FL LFalse <? f)
         then return $ FL LFalse
         else return $ FB BBoolean

tLen :: Expr -> TypeState F
tLen (ExpUnaryOp Hash e1) = do
    TF f <- getTypeExp e1
    if f <? FB BString || f <? FTable [] Closed
    then return $ FB BInt
    else if f == FAny
         then return FAny 
         else throwError "tLen cannot typecheck"


closeAll :: TypeState ()
closeAll = do
  env <- get
  let closeEnv gm = fmap wrappedClose gm 
      wrappedClose (TF f) = TF $ close f
      wrappedClose x = x
      gammaStack = fmap closeEnv (env ^. gamma)
  put $ Env gammaStack (env ^. pi) (env ^. counter) (env ^. assumpt)  


closeSet :: [String] -> TypeState ()
closeSet nms = do
  env <- get
  let gammaMap = env ^. gamma 
      wrappedClose nms key (TF f) = if key `elem` nms 
                                    then Just . TF . close $ f
                                    else Just . TF $ f
      wrappedClose nms key t = Just t
      closedStack = fmap (mapMaybeWithKey (wrappedClose nms)) gammaMap
  put $ Env closedStack (env ^. pi) (env ^. counter) (env ^. assumpt)   

openSet :: [String] -> TypeState ()
openSet nms = do
  env <- get
  let gammaMap = env ^. gamma
      wrappedOpen nms key (TF f) = if key `elem` nms 
                                   then Just . TF . open $ f
                                   else Just . TF $ f
      wrappedOpen nms key t = Just t
      openedStack = fmap (mapMaybeWithKey (wrappedOpen nms)) gammaMap
  put $ Env openedStack (env ^. pi) (env ^. counter) (env ^. assumpt)  