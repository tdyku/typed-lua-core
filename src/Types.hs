module Types where


data F = FL L
       | FB B
       | FNil
       | FValue
       | FAny
       | FSelf
       | FUnion [F]
       | FFunction S S
       | FTable [(F,V)] TType
       | FVariable String
       | FRecursive String F
       deriving Show

data L = LFalse | LTrue | LInt Int | LFloat Float | LString String deriving Show

data B = BBoolean | BInt | BNumber | BString deriving Show

data V = VF F
       | VConst F
       deriving Show

data S = SP P
       | SUnion [P]
       deriving Show

data P = P [F] (Maybe F) -- tuple type - list over F types possibly ending with variadic type
       deriving Show


data TType = Unique | Open | Fixed | Closed deriving Show