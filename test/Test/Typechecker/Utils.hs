{-# LANGUAGE LambdaCase #-}
module Test.Typechecker.Utils where

import           Text.Trifecta.Parser  (parseString)
import           Text.Trifecta.Result  (Result(..))

import Parser.Code           (pManyStm)
import Transform.Globals     (runGlobalTransform)
import Typechecker.Utils     (runTypechecker)
import Typechecker.Type      (tBlock)
import Parser.Comments       (preCompilation)

typeCheck :: String -> IO (Either String ())
typeCheck fpath = do
        withoutComments <- preCompilation fpath
        case parseString pManyStm mempty withoutComments of
            Failure _ -> return . Left $ "Cannot parse"
            Success res -> do
                transformedRes <- runGlobalTransform res
                runTypechecker transformedRes tBlock