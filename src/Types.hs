module Types where


data F = FL L                   --literal
       | FB B                   --base
       | FNil                   --nil
       | FValue                 --value
       | FAny                   --any
       | FSelf                  --self
       | FUnion [F]             --union
       | FFunction S S          --function
       | FTable TypedList TType --table
       | FVariable String       --TODO: variable type
       | FRecursive String F    --TODO: recursive type
       deriving (Show, Eq)


-- literal types
data L = LFalse | LTrue | LInt Int | LFloat Float | LString String deriving (Show, Eq)


-- basic types
data B = BBoolean | BInt | BNumber | BString deriving (Show, Eq)


-- value type
data V = VF F
       | VConst F
       deriving (Show, Eq)


-- snd level type
data S = SP P       -- tuple types
       | SUnion [P] -- union of tuples
       deriving (Show, Eq)

data P = P [F] (Maybe F) -- tuple type - list over F types possibly ending with variadic type
       deriving (Show, Eq)


data TType = Unique | Open | Fixed | Closed deriving (Show, Eq)
type TypedList = [(F,V)]