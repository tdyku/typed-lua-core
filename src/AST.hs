module AST where

import Types (S,F,V)

data Block = Block [Stm] deriving Show

data Stm = Skip                                     -- many statements              
         | StmAssign [LHVal] ExprList                        -- [l] = el
         | StmWhile Expr Block                      -- while e do s
         | StmIf Expr Block Block                   -- if e then s1 else s2
         | StmTypedVarDecl [IdType] ExprList Block    -- local id:F+ = el in s
         | StmVarDecl [Id] ExprList Block             -- local id    = el in s
         | StmRecDecl IdType Expr Block             -- rec:F       = e  in s
         | StmReturn ExprList                          -- return el
         | StmVoidAppl Appl                         -- |a|0
         | StmMthdDecl Id Id ParamList S Block      -- fun id1:id2 (pl):S s; return el
         deriving Show


data Expr = ExpNil                         -- nil
          | ExpInt Int                     -- int
          | ExpFloat Float                 -- float
          | ExpString String               -- string
          | ExpFalse                       -- false
          | ExpTrue                        -- true
          | ExpVar Id                      -- id
          | ExpTableAccess Expr Expr         -- TODO: e1[e2]
          | ExpTypeCoercion F Id           -- <F> id
          | ExpFunDecl ParamList S Block   -- fund
          | ExpTableConstructor TableList (Maybe Appl) -- {[e1] = e2+} | {[e1] = e2+, me}
          | ExpABinOp AOp Expr Expr        -- e1 + e2, e1 .. e2, e1 == e2, e1 < e2           
          | ExpBBinOp BOp Expr Expr        -- e1 & e2, e1 and e2, e1 or e2
          | ExpUnaryOp UnOp Expr           -- not e, # e
          | ExpOneResult Appl              -- |me|1
          deriving Show

data LHVal = IdVal Id                      -- id
           | TableVal Id Expr              -- e1[e2]
           | TypeCoercionVal Id Expr V     -- id[e]<V>
           deriving Show

data ExprList = ExprList [Expr] (Maybe Appl)
    deriving Show

data Appl = FunAppl Expr ExprList                 -- e(el)
          | MthdAppl Expr Id ExprList              -- e:n(el)
          | VarArg
          deriving Show           

data ParamList = ParamList [(Id, F)] (Maybe F) deriving Show  -- id:F+ | id:F+, ...:F

type IdType = (Id, F)
type TableList = [(Expr, Expr)]
type Id = String
data AOp = Add | Concat | Equals | LessThan | Div | Mod | IntDiv deriving Show
data BOp = Amp | And | Or deriving Show
data UnOp = Hash | Not deriving Show
