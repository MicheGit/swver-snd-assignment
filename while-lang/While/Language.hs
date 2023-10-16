module While.Language where
import GHC.Natural (Natural)

data AExp
    = Nat Natural
    | Var String
    | Neg AExp
    | Sum AExp AExp
    | Sub AExp AExp
    | Mul AExp AExp
    | Div AExp AExp
    | Inc String
    | Dec String
    | PrefixInc String
    | PrefixDec String
    deriving (Eq, Show)

-- the higher the number, the most priority it has
-- precedence levels are from https://en.cppreference.com/w/cpp/language/operator_precedence

maxPriorityLevel :: Int
maxPriorityLevel = 17

data BinOp a = BinOp
    { prio :: Int
    , term :: a -> a -> a
    }

prec :: BinOp a -> Int
prec binop = maxPriorityLevel - prio binop

operMul :: BinOp AExp
operMul = BinOp
    { prio = 5
    , term = Mul
    }

operSum :: BinOp AExp
operSum = BinOp
    { prio = 6
    , term = Sum
    }

operSub :: BinOp AExp
operSub = BinOp
    { prio = 6
    , term = Sub
    }

operDiv :: BinOp AExp
operDiv = BinOp
    { prio = 5
    , term = Div
    }

data BExp
    = Lit Bool
    | Not Bool
    | And BExp BExp
    | Or  BExp BExp
    | Eq  AExp AExp
    | Neq AExp AExp
    | Low AExp AExp
    | Leq AExp AExp
    | Grt AExp AExp
    | Geq AExp AExp

operAnd :: BinOp BExp
operAnd = BinOp
    { prio = 14
    , term = And
    }

operOr :: BinOp BExp
operOr = BinOp
    { prio = 15
    , term = Or
    }