{-# LANGUAGE TemplateHaskell #-}
module Typechecker.Utils where


import Control.Monad.State      (State, StateT, liftIO, get, put, evalStateT)
import Control.Monad.Except     (ExceptT, throwError, runExceptT)
import Control.Lens
import Types                    (F, T(..), P(..), S(..), E(..))
import Data.Map
import Data.Map                 (lookup)
import Prelude                   hiding (pi, lookup)

type Name = String

data Env = Env {
    _gamma   :: Map Name T,
    _pi      :: Map Int  S,
    _counter :: Int
}
makeLenses ''Env

type TypeState a = ExceptT String (StateT Env IO) a

runTypechecker y p = evalStateT (runExceptT $ p y) (Env empty empty 0) 

tic :: TypeState Int
tic = do
    env <- get
    put $ Env (env ^. gamma) (env ^. pi) (env ^. counter + 1)
    return $ env ^. counter

lookupGamma :: String -> TypeState T
lookupGamma var = do
    env <- get 
    case lookup var (env ^. gamma) of
        Just tp -> return tp
        Nothing -> throwError $ "Cannot find " ++ var ++ " in gamma."

insertSToPi :: Int -> S -> TypeState ()
insertSToPi i s = do
    env <- get
    let newPI = insert i s (env ^. pi)
    put $ Env (env ^. gamma) newPI (env ^. counter)

insertToGamma :: String -> T -> TypeState ()
insertToGamma id tp = do
    env <- get
    let newGamma = insert id tp (env ^. gamma)
    put $ Env newGamma (env ^. pi) (env ^. counter)


tlog :: (Show a) => a -> TypeState ()
tlog = liftIO . putStrLn . show

e2s :: E -> TypeState S
e2s (E ts mb) = do
    fs <- mapM unwrap ts
    if mb == Nothing 
    then return (SP $ P fs Nothing)
    else do mbF <- mapM unwrap mb
            return (SP $ P fs mbF)
    where unwrap :: T -> TypeState F
          unwrap (TF f) = return f
          unwrap _      = throwError "Cannot convert projection type to first level type."

