{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}


module Transform.Globals where

import Control.Lens
import Control.Monad.State.Lazy

import AST (Block(..), Stm(..), Expr(..), Appl(..), ExprList(..), ParamList(..), LHVal(..))


data Locals = Locals {
    _locals :: [[String]]
}
makeLenses ''Locals


type GlobalTransform a = StateT Locals IO a

runGlobalTransform b = evalStateT (transformBlock b) (Locals [[]])


pushNewScope :: [String] -> GlobalTransform ()
pushNewScope nms = do
    scopes <- get
    put . Locals $ nms : scopes ^. locals


popScope :: GlobalTransform ()
popScope = do
    scopes <- get
    let s:ss = scopes ^. locals
    put . Locals $ ss


isLocal :: String -> GlobalTransform Bool
isLocal nm = do
    scopes <- get
    return $ anyT $ fmap (nm `elem`) (scopes ^. locals)
    where anyT = elem True


transformBlock :: Block -> GlobalTransform Block
transformBlock (Block stms) = do
    stms' <- mapM transformStm stms
    return $ Block stms'


transformStm :: Stm -> GlobalTransform Stm
transformStm Skip = return Skip

transformStm (StmRecDecl (id, tp) exp blk) = do
    exp' <- transformExpr exp
    pushNewScope [id]
    blk' <- transformBlock blk
    popScope
    return $ StmRecDecl (id, tp) exp' blk'

transformStm (StmVarDecl ids exps blk) = do
    exps' <- transformExprList exps
    pushNewScope ids
    blk' <- transformBlock blk
    popScope
    return $ StmVarDecl ids exps' blk'

transformStm (StmTypedVarDecl tids exps blk) = do
    exps' <- transformExprList exps
    pushNewScope (fst <$> tids)
    blk' <- transformBlock blk
    popScope
    return $ StmTypedVarDecl tids exps' blk'

transformStm (StmReturn exps) = do
    exps' <- transformExprList exps
    return . StmReturn $ exps'

transformStm (StmVoidAppl app) = do
    app' <- transformAppl app
    return . StmVoidAppl $ app'

transformStm (StmMthdDecl tabId mthdId p@(ParamList params _) s blk) = do
    pushNewScope ((fst <$> params) ++ ["..."])
    blk' <- transformBlock blk
    popScope
    return $ StmMthdDecl tabId mthdId p s blk'

transformStm (StmWhile exp blk) = do
    exp' <- transformExpr exp
    blk' <- transformBlock blk
    return $ StmWhile exp' blk'

transformStm (StmIf exp tBlk eBlk) = do
    exp' <- transformExpr exp
    tBlk' <- transformBlock tBlk
    eBlk' <- transformBlock eBlk
    return $ StmIf exp' tBlk' eBlk'

transformStm (StmAssign lhs exps) = do
    exps' <- transformExprList exps
    lhs' <- transformLHS lhs
    return $ StmAssign lhs' exps'


transformExpr :: Expr -> GlobalTransform Expr
transformExpr ExpNil = return ExpNil
transformExpr i@(ExpInt _) = return i
transformExpr f@(ExpFloat _) = return f
transformExpr s@(ExpString _) = return s
transformExpr ExpFalse = return ExpFalse
transformExpr ExpTrue = return ExpTrue

transformExpr v@(ExpVar id) = 
    isLocal id >>= \case 
          True -> return v
          False -> if id == "setmetatable" 
                   then return v
                   else return $ ExpTableAccess (ExpVar "_ENV") (ExpString id)

transformExpr (ExpTableAccess tab el) = do
    tab' <- transformExpr tab
    el' <- transformExpr el
    return $ ExpTableAccess tab' el'

transformExpr c@(ExpTypeCoercion f id) = return c   -- this should be rethinked twice

transformExpr (ExpFunDecl p@(ParamList params _) s blk) = do
    pushNewScope ((fst <$> params) ++ ["..."])
    blk' <- transformBlock blk
    popScope
    return $ ExpFunDecl p s blk'

transformExpr (ExpTableConstructor tls mApp) = do
    tls' <- mapM transTuple tls
    mApp' <- mapM transformAppl mApp
    return $ ExpTableConstructor tls' mApp'
    where transTuple (e1,e2) = do
            e1' <- transformExpr e1
            e2' <- transformExpr e2
            return (e1', e2')

transformExpr (ExpABinOp op e1 e2) = do
    e1' <- transformExpr e1
    e2' <- transformExpr e2
    return $ ExpABinOp op e1' e2'

transformExpr (ExpBBinOp op e1 e2) = do
    e1' <- transformExpr e1
    e2' <- transformExpr e2
    return $ ExpBBinOp op e1' e2'

transformExpr (ExpUnaryOp op e) = do
    e' <- transformExpr e
    return $ ExpUnaryOp op e'

transformExpr (ExpOneResult app) = do
   app' <- transformAppl app
   return $ ExpOneResult app'   


transformExprList :: ExprList -> GlobalTransform ExprList
transformExprList (ExprList exps mApp) = do
    exps' <- mapM transformExpr exps
    mApp' <- mapM transformAppl mApp
    return $ ExprList exps' mApp'

transformAppl :: Appl -> GlobalTransform Appl
transformAppl VarArg = return VarArg

transformAppl (FunAppl exp expList) = do
    exp' <- transformExpr exp
    expList' <- transformExprList expList
    return $ FunAppl exp' expList'

transformAppl (MthdAppl exp id expList) = do
    exp' <- transformExpr exp
    expList' <- transformExprList expList
    return $ MthdAppl exp' id expList'


transformLHS :: [LHVal] -> GlobalTransform [LHVal]
transformLHS = mapM trFun
    where trFun (TableVal id exp) = transformExpr exp >>= return . TableVal id
          trFun (TypeCoercionVal id exp v) = transformExpr exp >>= \x -> return $ TypeCoercionVal id x v
          trFun (IdVal id) = isLocal id >>= \case
            True -> return . IdVal $ id
            False -> return $ TableVal "_ENV" (ExpString id)
