module Parser.Code where

--import           Text.Parsec.String                       (parseFromFile)
--import           Text.Parsec                              (Parsec)
--import           Text.Parsec.Char                         (letter, lower, alphaNum, string, digit, char, oneOf)
--import           Text.ParserCombinators.Parsec.Combinator (choice, between, sepBy, sepBy1)
--import           Text.ParserCombinators.Parsec            (Parser, many, many1, spaces, space, optionMaybe)
--import           Control.Applicative                      ((<*>), (*>), pure)
--import           Text.Parsec.Prim                         (try, (<?>))

--import qualified AST               as A
--import           Parser.Types      (pS, pF, pV, idVar, (<:>), (<++>))
--import           Debug.Trace       (trace)

--import System.Exit

--parse :: IO [A.Stm]
--parse = parseFromFile pManyStm "example" >>= either report return
--  where
--    report err = do
--        putStrLn $ "Error: " ++ show err
--        return []

--keyword :: String -> Parser ()
--keyword x = string x >> spaces

--symbol :: Char -> Parser ()
--symbol x = char x >> spaces


--semicolon, comma, spaces1 :: Parser ()
--semicolon = char ';' >> spaces
--comma = char ',' >> spaces
--spaces1 = space >> spaces

--pStm, pSSkip,  pSAssign, pSWhile, pSIf, pSTypedDecl, pSDecl, pSRecDecl, pSReturn, pSVoidAppl, pSMthDecl :: Parser A.Stm

--pStm | trace ("myfun ") True = choice [pSWhile, pSSkip, pSIf, pSTypedDecl{-, pSDecl, pSAssign, pSRecDecl, pSReturn, pSVoidAppl, pSMthDecl-}] <?> "pStm"
--pManyStm = pStm `sepBy` semicolon 
--pSSkip = keyword "skip" *> pure A.Skip

--pSAssign = A.StmAssign <$> pLHVal `sepBy` semicolon <*>( symbol '=' *> pExprList)

--pSIf = do
--	keyword "if"
--	e <- pExpr
--	keyword "then"
--	stm1 <- pManyStm
--	keyword "else"
--	stm2 <- pManyStm
--	return $ A.StmIf e stm1 stm2

--pSWhile = do
--	keyword "while"
--	e <- pExpr
--	keyword "do"
--	stm1 <- pManyStm
--	return $ A.StmWhile e stm1

--pSTypedDecl = do
--	keyword "local"
--	typedVars <- ((,) <$> idVar <* symbol ':' <*> pF ) `sepBy` semicolon
--	symbol '='
--	exprList <- pExprList
--	keyword "in"
--	stm <- pManyStm
--	return $ A.StmTypedVarDecl typedVars exprList stm

--pSDecl = do
--	keyword "local"
--	ids <- idVar `sepBy` semicolon
--	symbol '='
--	exprList <- pExprList
--	keyword "in"
--	stm <- pManyStm
--	return $ A.StmVarDecl ids exprList stm


--pSRecDecl = do
--	keyword "rec"
--	id <- idVar
--	symbol ':'
--	idType <- pF
--	symbol '='
--	expr <- pExpr
--	keyword "in"
--	stm <- pManyStm
--	return $ A.StmRecDecl (id, idType) expr stm

--pSReturn = keyword "return" *> (A.StmReturn <$> pExprList)

--pSVoidAppl = A.StmVoidAppl <$> between (symbol '|') (symbol '|') pA 

--pSMthDecl = do
--	keyword "fun"
--	id1 <- idVar
--	symbol ':'
--	id2 <- idVar
--	args <- between (char '(') (char ')') pPL
--	symbol ':'
--	retType <- pS
--	body <- pManyStm
--	return $ A.StmMthdDecl id1 id2 args retType body


--pExpr, pExpNil, pExpInt, pExpFloat, pExpString, pExpFalse, pExpTrue, pExpVar, pExpTableAccess, pExpTypeCoercion, pExpFunDecl, pExpTableConstructor, pExpABinOp, pExpBBinOp, pExpUnary, pExpOneResult :: Parser A.Expr

--pExpr = choice [try pExpNil, try pExpFloat, pExpInt,  pExpString, pExpFalse, pExpTrue, try pExpTableAccess, try pExpTypeCoercion, pExpFunDecl, try pExpTableConstructor{-, pExpABinOp, pExpBBinOp-}, pExpUnary, pExpOneResult, pExpVar] <* spaces <?> "pExpr"
--pExpNil = keyword "nil" *> pure A.ExpNil <* spaces1
--pExpInt = A.ExpInt <$> read <$> many1 digit
--pExpFloat = A.ExpFloat <$> read <$> many1 digit <++> (char '.' <:> many1 digit)
--pExpString = char '\"' *> (A.ExpString <$> many alphaNum) <* symbol '\"'
--pExpFalse = keyword "false" *> pure A.ExpFalse
--pExpTrue = keyword "true" *> pure A.ExpTrue
--pExpVar = A.ExpVar <$> idVar
--pExpTableAccess = A.ExpTableAccess <$> idVar <*> between (symbol '[') (symbol ']') pExpr
--pExpTypeCoercion = A.ExpTypeCoercion <$> between (symbol '<' ) (symbol '>') pF <*> idVar
--pExpFunDecl = do
--	keyword "fun"
--	args <- between (symbol '(') (symbol ')') pPL
--	symbol ':'
--	retType <- pS
--	body <- pManyStm
--	return $ A.ExpFunDecl args retType body


--pExpTableConstructor = do
--	symbol '{'
--	exprs <- ((,) <$> between (symbol '[') (symbol ']') pExpr <* char '=' <*> pExpr) `sepBy` semicolon
--	me <- optionMaybe (semicolon *> pME)
--	symbol '}'
--	return $ A.ExpTableConstructor exprs me


--pExpABinOp = do
--	e1 <- pExpr
--	op <- choice [keyword "+" *> pure A.Add, keyword ".." *> pure A.Concat, keyword "==" *> pure A.Equals, keyword "<" *> pure A.LessThan]
--	e2 <- pExpr
--	return $ A.ExpABinOp op e1 e2



--pExpBBinOp = do
--	e1 <- pExpr
--	op <- choice [keyword "&" *> pure A.Amp, keyword "and" *> pure A.And, keyword "or" *> pure A.Or]
--	e2 <- pExpr
--	return $ A.ExpBBinOp op e1 e2

--pExpUnary = A.ExpUnaryOp <$> choice [keyword "not" *> pure A.Not, keyword "#" *> pure A.Hash] <*> pExpr

--pExpOneResult = A.ExpOneResult <$> between (symbol '|') (symbol '|') pME

--pLHVal, pLHId, pTableVal, pTypeCoercionVal :: Parser A.LHVal
--pLHVal = choice [pLHId, pTableVal, pTypeCoercionVal]
--pLHId = A.IdVal <$> idVar
--pTableVal = A.TableVal <$> idVar <*> between (symbol '[') (symbol ']') pExpr
--pTypeCoercionVal = A.TypeCoercionVal <$> idVar <*> between (symbol '[') (symbol ']') pExpr <*> between (symbol '<') (symbol '>') pV


--pExprList :: Parser A.ExprList
--pExprList = A.ExprList <$> pExpr `sepBy` semicolon <*> optionMaybe (semicolon *> pME)


--pME :: Parser A.MultResult
--pME = choice [A.ResultAppl <$> pA, keyword "..." *> pure A.ResultVarArg]


--pA, pFunApp, pMthdApp :: Parser A.Appl
--pA = choice [pFunApp, pMthdApp]
--pFunApp = A.FunAppl <$> pExpr <*> between (symbol '(') (symbol ')') pExprList
--pMthdApp = A.MthdAppl <$> pExpr <* symbol ':' <*> idVar <*> between (symbol '(') (symbol ')') pExprList

--pPL :: Parser A.ParamList
--pPL = A.ParamList <$> ((,) <$> idVar <* symbol ':' <*> pF) `sepBy` semicolon <*> (optionMaybe $ keyword "..." *> symbol ':' *> pF)