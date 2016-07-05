{-# LANGUAGE TemplateHaskell #-}
module Typechecker.Utils where


import Control.Monad.State      (State, StateT, liftIO, get, put, evalStateT)
import Control.Monad.Except     (ExceptT, throwError, runExceptT)
import Control.Lens
import Types                    (F(..), T(..), P(..), S(..), E(..))
import Data.Map
import Prelude                   hiding (pi, lookup)
import Data.Maybe               (fromMaybe, isNothing)


allT, anyT :: [Bool] -> Bool
allT = all (== True)
anyT = any (== True)


type Name = String


data Env = Env {
    _gamma   :: [Map Name T],
    _pi      :: [Map Int  S],
    _counter :: Int
}
makeLenses ''Env

type TypeState a = ExceptT String (StateT Env IO) a

runTypechecker y p = evalStateT (runExceptT $ p y) (Env [empty] [empty] 0) 

tic :: TypeState Int
tic = do
    env <- get
    put $ Env (env ^. gamma) (env ^. pi) (env ^. counter + 1)
    return $ env ^. counter

lookupGamma :: String -> TypeState T
lookupGamma var = do
    env <- get 
    lookfor var (env ^. gamma)
  where lookfor :: String -> [Map Name T] -> TypeState T
        lookfor var (m:ms) = case lookup var m of
                                Just tp -> return tp
                                Nothing -> lookfor var ms
        lookfor var [] = throwError $  "Cannot find " ++ var ++ " in gamma."

lookupPI :: Int -> TypeState S
lookupPI x = do
    env <- get 
    lookfor x (env ^. pi)
    where lookfor :: Int -> [Map Int S] -> TypeState S
          lookfor x (m:ms) = case lookup x m of
                                  Just tp -> return tp
                                  Nothing -> lookfor x ms
          lookfor x [] = throwError $  "Cannot find " ++ show x ++ " in pi." 

insertSToPi :: Int -> S -> TypeState ()
insertSToPi i s = do
    env <- get
    let (piMap:piMaps) = env ^. pi
        newPI = insert i s piMap
    put $ Env (env ^. gamma) (newPI:piMaps) (env ^. counter)

insertToGamma :: String -> T -> TypeState ()
insertToGamma id tp = do
    env <- get
    let (gMap:gMaps) = env ^. gamma
        newGamma = insert id tp gMap
    put $ Env (newGamma:gMaps) (env ^. pi) (env ^. counter)

newGammaScope :: TypeState ()
newGammaScope = do
    env <- get
    put $ Env (mempty : env ^. gamma) (env ^. pi) (env ^. counter)


newPiScope :: TypeState ()
newPiScope = do
    env <- get
    put $ Env (env ^. gamma) (mempty : env ^. pi) (env ^. counter)


newScopes :: TypeState ()
newScopes = do
    env <- get
    put $ Env (mempty : env ^. gamma) (mempty : env ^. pi) (env ^. counter)


popGammaScope :: TypeState ()
popGammaScope = do
    env <- get
    put $ Env (tail $ env ^. gamma) (env ^. pi) (env ^. counter)


popPiScope :: TypeState ()
popPiScope = do
    env <- get
    put $ Env (env ^. gamma) (tail $ env ^. pi) (env ^. counter)


popScopes :: TypeState ()
popScopes = do
    env <- get
    put $ Env (tail $ env ^. gamma) (tail $ env ^. pi) (env ^. counter)   


tlog :: (Show a) => a -> TypeState ()
tlog = liftIO . print

e2s :: E -> TypeState S
e2s (E ts mb) = do
    fs <- mapM unwrap ts
    if isNothing mb
    then return (SP $ P fs Nothing)
    else do mbF <- mapM unwrap mb
            return (SP $ P fs mbF)
    where unwrap :: T -> TypeState F
          unwrap (TF f) = return f
          unwrap (TFilter _ f) = return f
          unwrap (TProj x i) = do
            piProj <- lookupPI x
            return $ case piProj of
                SP p@(P fs mf) -> unwrapP i p
                SUnion ps -> FUnion $ fmap (unwrapP i) ps
          
          unwrapP i (P fs mf) = if i < length fs 
                                then fs !! i
                                else fromMaybe FNil mf


sp2e :: S -> E
sp2e (SP (P fs mf)) = E (fmap TF fs) (fmap TF mf)


s2f :: S -> ([F], F)
s2f (SP (P fs mf)) = (fs, fromMaybe FNil mf) 