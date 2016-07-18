{-# LANGUAGE TemplateHaskell #-}
module Typechecker.Utils where


import Control.Monad.State   (State, StateT, liftIO, get, put, evalStateT)
import Control.Monad.Except  (ExceptT, throwError, runExceptT)
import Control.Lens
import Types                 (F(..), T(..), P(..), S(..), E(..))
import Data.Map
import Prelude               hiding (pi, lookup)
import Data.Maybe            (fromMaybe, isNothing)
import Data.List             (nub)
import Text.Show.Pretty      (ppShow)


allT, anyT :: [Bool] -> Bool
allT = all (== True)
anyT = any (== True)


type Name = String


data Env = Env {
    _gamma   :: [Map Name T],
    _pi      :: [Map Int  S],
    _counter :: Int,
    _assumpt :: [(F, F)]
}
makeLenses ''Env

type TypeState a = ExceptT String (StateT Env IO) a

runTypechecker y p = evalStateT (runExceptT $ p y) (Env [empty] [empty] 0 []) 

tic :: TypeState Int
tic = do
    env <- get
    put $ Env (env ^. gamma) (env ^. pi) (env ^. counter + 1) (env ^. assumpt)
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
    put $ Env (env ^. gamma) (newPI:piMaps) (env ^. counter) (env ^. assumpt)

insertToGamma :: String -> T -> TypeState ()
insertToGamma id tp = do
    env <- get
    let (gMap:gMaps) = env ^. gamma
        newGamma = insert id tp gMap
    tlog $ "Inserting: " ++ id ++ " of type: " ++ ppShow tp
    put $ Env (newGamma:gMaps) (env ^. pi) (env ^. counter) (env ^. assumpt)

newGammaScope :: TypeState ()
newGammaScope = do
    env <- get
    put $ Env (mempty : env ^. gamma) (env ^. pi) (env ^. counter) (env ^. assumpt)


getGamma :: TypeState (Map Name T)
getGamma = do
    env <- get
    return $ env ^. gamma ^?! _head


insertGamma :: Map Name T -> TypeState ()
insertGamma gm = do
    env <- get
    put $ Env (gm : env ^. gamma) (env ^. pi) (env ^. counter) (env ^. assumpt)

newPiScope :: TypeState ()
newPiScope = do
    env <- get
    put $ Env (env ^. gamma) (mempty : env ^. pi) (env ^. counter) (env ^. assumpt)


newScopes :: TypeState ()
newScopes = do
    env <- get
    put $ Env (mempty : env ^. gamma) (mempty : env ^. pi) (env ^. counter) (env ^. assumpt)


popGammaScope :: TypeState ()
popGammaScope = do
    env <- get
    put $ Env (tail $ env ^. gamma) (env ^. pi) (env ^. counter) (env ^. assumpt)


popPiScope :: TypeState ()
popPiScope = do
    env <- get
    put $ Env (env ^. gamma) (tail $ env ^. pi) (env ^. counter) (env ^. assumpt)


popScopes :: TypeState ()
popScopes = do
    env <- get
    put $ Env (tail $ env ^. gamma) (tail $ env ^. pi) (env ^. counter) (env ^. assumpt)  


tlog :: (Show a) => a -> TypeState ()
tlog = liftIO . putStrLn . ppShow

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



getFsFromP :: P -> [F]
getFsFromP (P fs _) = fs

getMFromP :: P -> F
getMFromP (P _ mf) = fromMaybe FNil mf


s2f :: S -> ([F], F)
s2f (SP (P fs mf)) = (fs, fromMaybe FNil mf) 
--s2f (SUnion ps) | length ps == 1 = s2f (SP . head $ ps)
s2f (SUnion ps) = 
    let pNum = length ps
        mLen = maximum $ fmap (length . getFsFromP) ps
        extent (P fs mf) = fs ++ replicate (mLen - (length fs)) (fromMaybe FNil mf)
        extended = fmap extent ps
        varargs = reverse . nub . reverse $ fmap getMFromP ps ++ [FNil]
        sVarargs = if length varargs == 1 then varargs !! 0 else FUnion varargs
        merge i n ms es | i == n = ms
                        | otherwise = merge (i+1) n (ms ++ [FUnion (fmap (!! i) es)]) es
    in  (merge 0 mLen [] extended, sVarargs)


