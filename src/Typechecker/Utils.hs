{-# LANGUAGE TemplateHaskell #-}
module Typechecker.Utils where


import Control.Monad.State      (State, StateT, liftIO, get, put, evalStateT)
import Control.Monad.Except     (ExceptT, throwError, runExceptT)
import Control.Lens
import Types                    (F, S)
import Data.Map

type Name = String

data Env = Env {
    _gamma :: Map Name F,
    _pi    :: Map Name S
}
makeLenses ''Env

type TypeState a = ExceptT String (StateT Env IO) a

runTypechecker y p = evalStateT (runExceptT $ p y) (Env empty empty) 

tlog :: (Show a) => a -> TypeState ()
tlog = liftIO . putStrLn . show

