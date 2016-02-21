module Parser.Utils where

import           Text.Parser.Char                         (letter, lower, alphaNum, string, digit, char, spaces, space)
import           Text.Parser.Combinators                  (option, choice, between, sepBy, sepBy1)
import           Text.Parser.Combinators                  (many) 
import           Control.Monad                            (liftM)
import           Control.Applicative                      ((<*>), (*>), pure)
import           Text.Parser.Combinators                  (try, (<?>))
import           Text.Trifecta.Parser                     (Parser)

(<:>) a b = (:) <$> a <*> b
(<++>) a b = (++) <$> a <*> b

keyword :: String -> Parser ()
keyword x = string x >> spaces

symbol :: Char -> Parser ()
symbol x = char x >> spaces


semicolon, comma, spaces1 :: Parser ()
semicolon = char ';' >> spaces
comma = char ',' >> spaces
spaces1 = space >> spaces

idVar :: Parser String
idVar = choice [letter, lower]  <:> many alphaNum <* spaces

optionMaybe p = option Nothing (liftM Just p)