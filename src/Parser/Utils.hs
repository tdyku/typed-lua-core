module Parser.Utils where

import           Text.Parsec                              (Parsec)
import           Text.Parsec.Char                         (letter, lower, alphaNum, string, digit, char, spaces, space)
import           Text.ParserCombinators.Parsec.Combinator (choice, between, sepBy, sepBy1)
import           Text.ParserCombinators.Parsec            (Parser, many, many1) 
import           Control.Applicative                      ((<*>), (*>), pure)
import           Text.Parsec.Prim                         (try, (<?>))

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