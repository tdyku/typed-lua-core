module AST where

import Types (S,F,V)

data Stm = Skip                                     -- skip               
         | StmAssign [LHVal] ExprList               -- [l] = el
         | StmWhile Expr [Stm]                      -- while e do s
         | StmIf Expr [Stm] [Stm]                   -- if e then s1 else s2
         | StmTypedVarDecl [IdType] ExprList [Stm]  -- local id:F+ = el in s
         | StmVarDecl [Id] ExprList [Stm]           -- local id    = el in s
         | StmRecDecl IdType Expr [Stm]             -- rec:F       = e  in s
         | StmReturn ExprList                       -- return el
         | StmVoidAppl Appl                         -- |a|0
         | StmMthdDecl Id Id ParamList S [Stm]      -- TODO: ExprList  -- fun id1:id2 (pl):S s; return el
         deriving Show


data Expr = ExpNil                         -- nil
          | ExpInt Int                     -- int
          | ExpFloat Float                 -- float
          | ExpString String               -- string
          | ExpFalse                       -- false
          | ExpTrue                        -- true
          | ExpVar Id                      -- id
          | ExpTableAccess Id Expr         -- e1[e2]
          | ExpTypeCoercion F Id           -- <F> id
          | ExpFunDecl ParamList S [Stm]   -- TODO: add ExprList -- f
          | ExpTableConstructor TableList (Maybe MultResult) -- {[e1] = e2+} | {[e1] = e2+, me}
          | ExpABinOp AOp Expr Expr        -- e1 + e2, e1 .. e2, e1 == e2, e1 < e2           
          | ExpBBinOp BOp Expr Expr        -- e1 & e2, e1 and e2, e1 or e2
          | ExpUnaryOp UnOp Expr           -- not e, # e
          | ExpOneResult MultResult    -- |me|1
          deriving Show

data LHVal = IdVal Id                      -- id
           | TableVal Id Expr            -- e1[e2]
           | TypeCoercionVal Id Expr V     -- id[e]<V>
           deriving Show

data ExprList = ExprList [Expr] (Maybe MultResult) deriving Show-- e+ | e+, me

data MultResult = ResultAppl Appl | ResultVarArg deriving Show  -- a | ...

data Appl = FunAppl Expr ExprList                  -- e(el)
          | MthdAppl Expr Id ExprList              -- e:n(el)
          deriving Show           

data ParamList = ParamList [(Id, F)] (Maybe F) deriving Show  -- id:F+ | id:F+, ...:F

type IdType = (Id, F)
type TableList = [(Expr, Expr)]
type Id = String
data AOp = Add | Concat | Equals | LessThan deriving Show
data BOp = Amp | And | Or deriving Show
data UnOp = Hash | Not deriving Show
