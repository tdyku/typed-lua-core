module Parser.Types where

import           Text.Parsec                              (Parsec)
import           Text.Parsec.Char                         (letter, lower, alphaNum, string, digit, char)
import           Text.ParserCombinators.Parsec.Combinator (choice, between, sepBy)
import           Text.ParserCombinators.Parsec            (Parser, many, many1) 
import           Control.Applicative                      ((<*>), (*>), pure)

import qualified AST               as A
import qualified Types             as T

(<:>) a b = (:) <$> a <*> b
(<++>) a b = (++) <$> a <*> b

idVar :: Parser A.Id
idVar = choice [letter, lower]  <:> (many alphaNum)



---- Types

-- literal types
pLitType, pFalseType, pTrueType, pInt, pFloat, pString :: Parser T.L
pLitType = choice [pFalseType, pTrueType, pInt, pFloat, pString]

pFalseType = string "false" *> return T.LFalse
pTrueType = string "true" *> return T.LTrue
pInt = T.LInt <$> read <$> many1 digit
pFloat = T.LFloat <$> read <$> many1 digit <++> (char '.' <:> many1 digit)
pString = char '\"' *> (T.LString <$> many alphaNum) <* char '\"'

-- base types
pBaseType, pBaseBool, pBaseInt, pBaseNumber, pBaseString :: Parser T.B

pBaseType = choice [pBaseBool, pBaseInt, pBaseNumber, pBaseString]
pBaseBool = string "boolean" *> pure T.BBoolean
pBaseInt = string "integer" *> pure T.BInt
pBaseNumber = string "number" *> pure T.BNumber
pBaseString = string "string" *> pure T.BString

-- V types
pV :: Parser T.V
pV = choice [string "const" *> (T.VConst <$> pF), T.VF <$> pF]

-- S types
pS :: Parser T.S
pS = choice [T.SP <$> pP, T.SUnion <$> pP `sepBy` char '|'] 

-- P type
pP :: Parser T.P
pP = T.P <$> (between (char ',') (char ',') (pF `sepBy` char ',')) <*> pure Nothing

-- F types
pF, pFL, pFB, pFNil, pFValue, pFAny, pFSelf, pFUnion, pFFunction, pFTable, pFVariable, pFRecursive :: Parser T.F

pF = choice [pFL, pFB, pFNil, pFValue, pFAny, pFSelf, pFUnion, pFFunction, pFTable, pFVariable, pFRecursive]

pFL = T.FL <$> pLitType
pFB = T.FB <$> pBaseType
pFNil = string "nil" *> pure T.FNil
pFValue = string "value" *>  pure T.FValue
pFAny = string "any" *> pure T.FAny
pFSelf = string "self" *> pure T.FSelf
pFUnion = T.FUnion <$> pF `sepBy` char '|'
pFFunction = T.FFunction <$> pS <* string "->" <*> pS
pFTable = T.FTable <$> between (char '{') (char '}') (many $ (,) <$> pF <* char ':' <*> pV) <*> pure T.Unique
pFVariable = T.FVariable <$> idVar	
pFRecursive = undefined