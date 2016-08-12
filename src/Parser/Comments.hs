module Parser.Comments where

import Text.Regex (subRegex, mkRegex)


preCompilation :: String -> IO String
preCompilation path = do
    sourceCode <- readFile path
    let regex = mkRegex "--.*\n"
    return $ subRegex regex sourceCode ""