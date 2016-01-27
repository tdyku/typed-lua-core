module Parser.Code where

import           Text.Parsec.String                       (parseFromFile)
import           Text.Parsec                              (Parsec)
import           Text.Parsec.Char                         (letter, lower, alphaNum, string, digit, char, oneOf)
import           Text.ParserCombinators.Parsec.Combinator (choice, between, sepBy, sepBy1)
import           Text.ParserCombinators.Parsec            (Parser, many, many1, spaces, optionMaybe)
import           Control.Applicative                      ((<*>), (*>), pure)

import qualified AST               as A
import           Parser.Types      (pS, pF, pV, idVar, (<:>), (<++>))

import System.Exit

parse :: IO A.Stm
parse = parseFromFile pStm "example" >>= either report return
  where
    report err = do
        putStrLn $ "Error: " ++ show err
        exitFailure


pStm, pSSkip, pSBlock, pSAssign, pSWhile, pSIf, pSTypedDecl, pSDecl, pSRecDecl, pSReturn, pSVoidAppl, pSMthDecl :: Parser A.Stm

pStm = choice [pSBlock, pSSkip,  pSAssign, pSWhile, pSIf, pSTypedDecl, pSDecl, pSRecDecl, pSReturn, pSVoidAppl, pSMthDecl]

pSSkip = string "skip" *> pure A.Skip

pSBlock = A.StmBlock <$> pStm `sepBy1` char ';'

pSAssign = A.StmAssign <$> pLHVal `sepBy` char ',' <*> pExprList

pSIf = do
	string "if"
	e <- pExpr
	string "then"
	stm1 <- pStm
	string "else"
	stm2 <- pStm
	return $ A.StmIf e stm1 stm2

pSWhile = do
	string "while"
	e <- pExpr
	string "do"
	stm1 <- pStm
	return $ A.StmWhile e stm1

pSTypedDecl = do
	string "local"
	typedVars <- ((,) <$> idVar <* char ':' <*> pF ) `sepBy` char ','
	char '='
	exprList <- pExprList
	string "in"
	stm <- pStm
	return $ A.StmTypedVarDecl typedVars exprList stm

pSDecl = do
	string "local"
	ids <- idVar `sepBy` char ','
	char '='
	exprList <- pExprList
	string "in"
	stm <- pStm
	return $ A.StmVarDecl ids exprList stm


pSRecDecl = do
	string "rec"
	id <- idVar
	char ':'
	idType <- pF
	char '='
	expr <- pExpr
	string "in"
	stm <- pStm
	return $ A.StmRecDecl (id, idType) expr stm

pSReturn = A.StmReturn <$> pExprList

pSVoidAppl = A.StmVoidAppl <$> between (char '|') (char '|') pA 

pSMthDecl = do
	string "fun"
	id1 <- idVar
	char ':'
	id2 <- idVar
	args <- between (char '(') (char ')') pPL
	char ':'
	retType <- pS
	body <- pStm
	return $ A.StmMthdDecl id1 id2 args retType body


pExpr, pExpNil, pExpInt, pExpFloat, pExpString, pExpFalse, pExpTrue, pExpVar, pExpTableAccess, pExpTypeCoercion, pExpFunDecl, pExpTableConstructor, pExpABinOp, pExpBBinOp, pExpUnary, pExpOneResult :: Parser A.Expr

pExpr = choice [pExpNil, pExpInt, pExpFloat, pExpString, pExpFalse, pExpTrue, pExpVar, pExpTableAccess, pExpTypeCoercion, pExpFunDecl, pExpTableConstructor, pExpABinOp, pExpBBinOp, pExpUnary, pExpOneResult]
pExpNil = string "nil" *> pure A.ExpNil
pExpInt = A.ExpInt <$> read <$> many1 digit
pExpFloat = A.ExpFloat <$> read <$> many1 digit <++> (char '.' <:> many1 digit)
pExpString = char '\"' *> (A.ExpString <$> many alphaNum) <* char '\"'
pExpFalse = string "false" *> pure A.ExpFalse
pExpTrue = string "true" *> pure A.ExpTrue
pExpVar = A.ExpVar <$> idVar
pExpTableAccess = A.ExpTableAccess <$> pExpr <*> between (char '[') (char ']') pExpr
pExpTypeCoercion = A.ExpTypeCoercion <$> between (char '<' ) (char '>') pF <*> idVar
pExpFunDecl = do
	string "fun"
	args <- between (char '(') (char ')') pPL
	char ':'
	retType <- pS
	body <- pStm
	return $ A.ExpFunDecl args retType body


pExpTableConstructor = do
	char '{'
	exprs <- ((,) <$> between (char '[') (char ']') pExpr <* char '=' <*> pExpr) `sepBy` char ','
	me <- optionMaybe (char ',' *> pME)
	char '}'
	return $ A.ExpTableConstructor exprs me


pExpABinOp = do
	e1 <- pExpr
	op <- choice [string "+" *> pure A.Add, string ".." *> pure A.Concat, string "==" *> pure A.Equals, string "<" *> pure A.LessThan]
	e2 <- pExpr
	return $ A.ExpABinOp op e1 e2



pExpBBinOp = do
	e1 <- pExpr
	op <- choice [string "&" *> pure A.Amp, string "and" *> pure A.And, string "or" *> pure A.Or]
	e2 <- pExpr
	return $ A.ExpBBinOp op e1 e2

pExpUnary = A.ExpUnaryOp <$> choice [string "not" *> pure A.Not, string "#" *> pure A.Hash] <*> pExpr

pExpOneResult = A.ExpOneResult <$> between (char '|') (char '|') pME

pLHVal, pLHId, pTableVal, pTypeCoercionVal :: Parser A.LHVal
pLHVal = choice [pLHId, pTableVal, pTypeCoercionVal]
pLHId = A.IdVal <$> idVar
pTableVal = A.TableVal <$> pExpr <*> between (char '[') (char ']') pExpr
pTypeCoercionVal = A.TypeCoercionVal <$> idVar <*> between (char '[') (char ']') pExpr <*> between (char '<') (char '>') pV


pExprList :: Parser A.ExprList
pExprList = A.ExprList <$> pExpr `sepBy` char ',' <*> optionMaybe (char ',' *> pME)


pME :: Parser A.MultResult
pME = choice [A.ResultAppl <$> pA, string "..." *> pure A.ResultVarArg]


pA, pFunApp, pMthdApp :: Parser A.Appl
pA = choice [pFunApp, pMthdApp]
pFunApp = A.FunAppl <$> pExpr <*> between (char '(') (char ')') pExprList
pMthdApp = A.MthdAppl <$> pExpr <* char ':' <*> idVar <*> between (char '(') (char ')') pExprList

pPL :: Parser A.ParamList
pPL = A.ParamList <$> ((,) <$> idVar <* char ':' <*> pF) `sepBy` char ',' <*> (optionMaybe $ string "..." *> char ':' *> pF)