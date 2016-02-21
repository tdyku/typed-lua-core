module Parser.Types where

import           Text.Parser.Char                         (alphaNum, digit, char, spaces)
import           Text.Parser.Combinators                  (choice, between, sepBy, option, many)
import           Text.Trifecta.Parser                     (Parser)
import           Control.Applicative                      ((<*>), (*>), pure)
import           Text.Parser.Combinators                  (try, (<?>))


import           Parser.Utils                             (keyword, symbol, (<++>), (<:>), comma, idVar, semicolon)
import qualified AST               as A
import qualified Types             as T


--parse :: IO T.F
--parse = parseFromFile pF "example" >>= either report return
--  where
--    report err = do
--        putStrLn $ "Error: " ++ show err
--        return T.FNil


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
pS = choice [T.SP <$> pP, T.SUnion <$> pP `sepBy` symbol '|']  <?> "pS"

-- P type
pP :: Parser T.P
pP = T.P <$> between (symbol '(') (symbol ')') (pF `sepBy` comma) <*> pure Nothing

-- F types
pF, pFPrim, pFL, pFB, pFNil, pFValue, pFAny, pFSelf, pFUnion, pFFunction, pFTable, pFVariable, pFRecursive :: Parser T.F

pF = choice [try pFUnion, pFPrim]
pFPrim = choice [pFL, pFB, pFValue, pFAny, pFSelf, pFNil, pFTable, pFFunction{- , pFVariable, pFRecursive-}]

pFL = T.FL <$> pLitType
pFB = T.FB <$> pBaseType
pFNil = keyword "nil" *> pure T.FNil
pFValue = keyword "value" *>  pure T.FValue
pFAny = keyword "any" *> pure T.FAny
pFSelf = keyword "self" *> pure T.FSelf
pFUnion =  T.FUnion <$> pFPrim <:> ((symbol '|' *> pFPrim) <:> many (symbol '|' *> pFPrim))
pFFunction =  T.FFunction <$> pS <* keyword "->" <*> pS <?> "pFFunction"
pFTable  =  T.FTable <$> between (symbol '{') (symbol '}') (((,) <$> pF <* symbol ':' <*> pV) `sepBy` comma) <*> option T.Unique pTableType
pFVariable = T.FVariable <$> idVar	
pFRecursive = undefined
pTableType = symbol '_' *> choice [ keyword "unique" *> pure T.Unique
                                  , keyword "fixed" *> pure T.Fixed
                                  , keyword "closed" *> pure T.Closed
                                  , keyword "open" *> pure T.Open]