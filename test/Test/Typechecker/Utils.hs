{-# LANGUAGE LambdaCase #-}
module Test.Typechecker.Utils where

import Text.Trifecta.Parser (parseString)
import Text.Trifecta.Result (Result(..))

import Parser.Code           (pManyStm)
import Transform.Globals     (runGlobalTransform)
import Typechecker.Utils     (runTypechecker)
import Typechecker.Type      (tBlock)

typeCheck :: String -> IO (Either String ())
typeCheck source = case parseString pManyStm mempty "skip" of
                        Failure a -> return . Left $ "Cannot parse"
                        Success res -> do
                            transformedRes <- runGlobalTransform res
                            runTypechecker transformedRes tBlock