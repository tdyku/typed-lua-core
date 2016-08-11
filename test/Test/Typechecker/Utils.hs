{-# LANGUAGE LambdaCase #-}
module Test.Typechecker.Utils where

import Text.Trifecta.Parser (parseFromFile)
import Text.Trifecta.Result (Result(..))

import Parser.Code           (pManyStm)
import Transform.Globals     (runGlobalTransform)
import Typechecker.Utils     (runTypechecker)
import Typechecker.Type      (tBlock)

typeCheck :: String -> IO (Either String ())
typeCheck path = parseFromFile pManyStm path >>= \case
                        Nothing -> return . Left $ "Cannot parse"
                        Just res -> do
                            transformedRes <- runGlobalTransform res
                            runTypechecker transformedRes tBlock