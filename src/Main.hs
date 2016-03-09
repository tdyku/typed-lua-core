{-# LANGUAGE LambdaCase #-}

import           Text.Trifecta.Parser (parseFromFile)
import           Parser.Code          (pManyStm)
import           Parser.Types
import           Text.Show.Pretty     (ppShow)
import           System.Environment   (getArgs)


main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> putStrLn "Error! Give path to core-typed-lua file"
        1 -> parse . head $ args
        _ -> putStrLn "Too much arguments. Give ONE path"

parse :: String -> IO ()
parse fpath = parseFromFile pManyStm fpath
          >>= putStrLn . \case 
                Nothing  -> ""
                Just res -> ppShow res
