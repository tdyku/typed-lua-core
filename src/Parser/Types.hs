module Parser.Types where

import           Text.Parser.Char                         (alphaNum, digit, char, spaces)
import           Text.Parser.Combinators                  (choice, between, sepBy, option, many, many)
import           Text.Trifecta.Parser                     (Parser)
import           Control.Applicative                      ((<*>), (*>), pure)
import           Text.Parser.Combinators                  (try, (<?>), sepBy1)


import           Parser.Utils                             (keyword, symbol, (<++>), (<:>), comma, idVar, semicolon, many1)
import qualified AST               as A
import qualified Types             as T

---- Types
-- literal types
pLitType, pFalseType, pTrueType, pInt, pFloat, pString :: Parser T.L
pLitType = choice [pFalseType, pTrueType, try pFloat, pInt, pString] <* spaces

pFalseType = keyword "false" *> return T.LFalse
pTrueType = keyword "true" *> return T.LTrue
pInt = T.LInt <$> read <$> digit <:> many digit
pFloat = T.LFloat <$> read <$> (digit <:> many digit) <++> (char '.' <:> (digit <:> many digit))
pString = char '\"' *> (T.LString <$> many alphaNum) <* symbol '\"'

-- base types
pBaseType, pBaseBool, pBaseInt, pBaseNumber, pBaseString :: Parser T.B

pBaseType = try $ choice [pBaseBool, pBaseInt, pBaseNumber, pBaseString]
pBaseBool = keyword "boolean" *> pure T.BBoolean
pBaseInt = keyword "integer" *> pure T.BInt
pBaseNumber = keyword "number" *> pure T.BNumber
pBaseString = keyword "string" *> pure T.BString

-- V types
pV :: Parser T.V
pV = choice [keyword "const" *> (T.VConst <$> pF), T.VF <$> pF]

-- S types
pS :: Parser T.S
pS = choice [try (T.SP <$> pP), T.SUnion <$> between (symbol '(') (symbol ')') (pP `sepBy1` symbol '|')] <?> "pS"

-- P type
pP :: Parser T.P
pP = T.P <$> between (symbol '(') (symbol ')') (pF `sepBy` comma) <*> pure Nothing

-- F types
pF, pFL, pFB, pFNil, pFValue, pFAny, pFSelf, pFUnion, pFFunction, pFTable, pFVariable, pFRecursive :: Parser T.F
pF = choice [pFL, pFB, pFValue, pFAny, pFSelf, pFNil, pFTable, try pFFunction, pFUnion, pFRecursive, pFVariable]

pFL = T.FL <$> pLitType
pFB = T.FB <$> pBaseType
pFNil = keyword "nil" *> pure T.FNil
pFValue = keyword "value" *>  pure T.FValue
pFAny = keyword "any" *> pure T.FAny
pFSelf = keyword "self" *> pure T.FSelf

pFUnion =  between (symbol '(') (symbol ')') (T.FUnion <$> pF <:> many1 (symbol '|' *> pF))
pFFunction =  T.FFunction <$> pS <* keyword "->" <*> pS <?> "pFFunction"
pFTable  =  T.FTable <$> between (symbol '{') (symbol '}') (((,) <$> pF <* symbol ':' <*> pV) `sepBy` comma) <*> option T.Unique pTableType
pFVariable = T.FVariable <$> idVar
pFRecursive = char 'u' *> (T.FRecursive <$> idVar <*> (char '.' *> pF)) 
pTableType = symbol '_' *> choice [ keyword "unique" *> pure T.Unique
                                  , keyword "fixed" *> pure T.Fixed
                                  , keyword "closed" *> pure T.Closed
                                  , keyword "open" *> pure T.Open]