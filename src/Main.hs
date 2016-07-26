{-# LANGUAGE LambdaCase #-}

import           Text.Trifecta.Parser (parseFromFile)
import           Text.Show.Pretty     (ppShow)
import           System.Environment   (getArgs)

import           Parser.Code          (pManyStm)
import           Parser.Types
import           Typechecker.Utils     (runTypechecker)
import           Typechecker.Type      (tBlock)
import           Transform.Globals     (runGlobalTransform)

main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> putStrLn "Error! Give path to core-typed-lua file"
        1 -> compile . head $ args
        _ -> putStrLn "Too much arguments. Give ONE path"

compile :: String -> IO ()
compile fpath = parseFromFile pManyStm fpath
          >>= \case 
              Nothing  -> return ()
              Just res -> do
                transformedRes <- runGlobalTransform res
                putStrLn . ppShow $ transformedRes
                checkedRes <- runTypechecker transformedRes tBlock
                case checkedRes of
                  Right () -> putStrLn "correct!"
                  Left err -> putStrLn err

