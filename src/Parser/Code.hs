module Parser.Code where

import           Text.Parser.Char                         (letter, space, lower, alphaNum, string, digit, char, oneOf, spaces)
import           Text.Parser.Combinators                  (choice, between, sepBy, sepBy1)
import           Text.Parser.Combinators                  (many, option, eof, notFollowedBy, chainl1)
import           Control.Applicative                      ((<*>), (*>), pure)
import           Text.Parser.Combinators                  (try, (<?>))
import           Text.Trifecta.Parser                     (Parser)

import qualified AST               as A
import           Parser.Types      (pS, pF, pV)
import           Parser.Utils      (idVar, (<:>), (<++>), keyword, semicolon, symbol, spaces1, comma, optionMaybe,)


pManyStm :: Parser [A.Stm]
pManyStm = pStm `sepBy` semicolon <* eof 

pStm, pSSkip,  pSAssign, pSWhile, pSIf, pSTypedDecl, pSDecl, pSRecDecl, pSReturn, pSVoidAppl, pSMthDecl :: Parser A.Stm
pStm = choice [pSWhile, pSSkip, pSIf, try pSDecl, try pSTypedDecl, try pSAssign, try pSRecDecl, pSReturn, pSVoidAppl, pSMthDecl] 
pSSkip = keyword "skip" *> pure A.Skip

pSAssign = A.StmAssign <$> pLHVal `sepBy` comma <*> ( symbol '=' *> pExprList)

pSIf = do
    keyword "if"
    e <- pExpr
    keyword "then"
    stm1 <- pManyStm
    keyword "else"
    stm2 <- pManyStm
    return $ A.StmIf e stm1 stm2

pSWhile = do
    keyword "while"
    e <- pExpr
    keyword "do"
    stm1 <- pManyStm
    return $ A.StmWhile e stm1

pSTypedDecl = do
    keyword "local"
    typedVars <- ((,) <$> idVar <* symbol ':' <*> pF ) `sepBy` comma
    symbol '='
    exprList <- pExprList
    keyword "in"
    stm <- pManyStm
    return $ A.StmTypedVarDecl typedVars exprList stm

pSDecl = do
    keyword "local"
    ids <- idVar `sepBy` comma
    symbol '='
    exprList <- pExprList
    keyword "in"
    stm <- pManyStm
    return $ A.StmVarDecl ids exprList stm


pSRecDecl = do
    keyword "rec"
    id <- idVar
    symbol ':'
    idType <- pF
    symbol '='
    expr <- pExpr
    keyword "in"
    stm <- pManyStm
    return $ A.StmRecDecl (id, idType) expr stm

pSReturn = keyword "return" *> (A.StmReturn <$> pExprList)

pSVoidAppl = A.StmVoidAppl <$> between (symbol '|') (symbol '|') pA 

pSMthDecl = do
    keyword "fun"
    id1 <- idVar
    symbol ':'
    id2 <- idVar
    args <- between (char '(') (char ')') pPL
    symbol ':'
    retType <- pS
    body <- pManyStm
    return $ A.StmMthdDecl id1 id2 args retType body


pExpr, pExpNil, pExpInt, pExpFloat, pExpString, pExpFalse, pExpTrue, pExpVar, pExpTableAccess, pExpTypeCoercion, pExpFunDecl, pExpTableConstructor, pExpABinOp, pExpUnary, pExpOneResult :: Parser A.Expr

pSimpleExp = choice [try pExpNil, try pExpFloat, try pExpInt,  pExpString, try pExpFalse, try pExpTrue, try pExpTableAccess, try pExpTypeCoercion, pExpFunDecl, try pExpTableConstructor,  pExpUnary, pExpOneResult, pExpVar] <* spaces
pExpr = choice [try pExpNil, try pExpABinOp, try pExpFloat, try pExpInt,  pExpString, try pExpFalse, try pExpTrue, try pExpTableAccess, try pExpTypeCoercion, pExpFunDecl, try pExpTableConstructor, pExpUnary, pExpOneResult, pExpVar] <* spaces
pExpNil = keyword "nil" *> pure A.ExpNil
pExpInt = A.ExpInt <$> read <$> (digit <:> many digit)
pExpFloat = A.ExpFloat <$> read <$> (digit <:> many digit) <++> (char '.' <:> (digit <:> many digit))
pExpString = char '\"' *> (A.ExpString <$> many alphaNum) <* symbol '\"'
pExpFalse = keyword "false" *> pure A.ExpFalse
pExpTrue = keyword "true" *> pure A.ExpTrue
pExpVar = A.ExpVar <$> idVar
pExpTableAccess = A.ExpTableAccess <$> idVar <*> between (symbol '[') (symbol ']') pExpr
pExpTypeCoercion = A.ExpTypeCoercion <$> between (symbol '<' ) (symbol '>') pF <*> idVar
pExpFunDecl = do
    keyword "fun"
    args <- between (symbol '(') (symbol ')') pPL
    symbol ':'
    retType <- pS
    body <- pManyStm
    return $ A.ExpFunDecl args retType body


pExpTableConstructor = do
    symbol '{'
    exprs <- option [] $ ((,) <$> between (symbol '[') (symbol ']') pExpr <* symbol '=' <*> pExpr) <:> many (try ( comma *> ((,) <$> between (symbol '[') (symbol ']') pExpr <* symbol '=' <*> pExpr)))
    me <- optionMaybe (comma *> pME)
    symbol '}'
    return $ A.ExpTableConstructor exprs me



pExpABinOp = pSimpleExp `chainl1` binOp
    where binOp = choice [ A.ExpABinOp <$> choice [ symbol '+'  *> pure A.Add 
                                                  , keyword ".." *> pure A.Concat
                                                  , keyword "==" *> pure A.Equals
                                                  , symbol '<' *> pure A.LessThan
                                                  ]
                         , A.ExpBBinOp <$> choice [ symbol '&'  *> pure A.Amp
                                                  , keyword "and" *> pure A.And
                                                  , keyword "or" *> pure A.Or
                                                  ]
                         ]


pExpUnary = A.ExpUnaryOp <$> choice [keyword "not" *> pure A.Not, keyword "#" *> pure A.Hash] <*> pExpr

pExpOneResult = A.ExpOneResult <$> between (symbol '|') (symbol '|') pME

pLHVal, pLHId, pTableVal, pTypeCoercionVal :: Parser A.LHVal
pLHVal = choice [try pTypeCoercionVal, try pTableVal, pLHId] <* spaces
pLHId = A.IdVal <$> idVar
pTableVal = A.TableVal <$> idVar <*> between (symbol '[') (symbol ']') pExpr
pTypeCoercionVal = A.TypeCoercionVal <$> idVar <*> between (symbol '[') (symbol ']') pExpr <*> between (symbol '<') (symbol '>') pV


pExprList :: Parser A.ExprList
pExprList = A.ExprList <$> pExpr <:> many (try (comma *> pExpr <* notFollowedBy (symbol '('))) <*> optionMaybe (comma *> pME)


pME :: Parser A.MultResult
pME = choice [keyword "..." *> pure A.ResultVarArg, A.ResultAppl <$> pA]


pA, pFunApp, pMthdApp :: Parser A.Appl
pA = choice [pFunApp, pMthdApp]
pFunApp = A.FunAppl <$> pExpr <*> between (symbol '(') (symbol ')') pExprList
pMthdApp = A.MthdAppl <$> pExpr <* symbol ':' <*> idVar <*> between (symbol '(') (symbol ')') pExprList

pPL :: Parser A.ParamList
pPL = A.ParamList <$> ((,) <$> idVar <* symbol ':' <*> pF) `sepBy` comma <*> optionMaybe (keyword "..." *> symbol ':' *> pF)