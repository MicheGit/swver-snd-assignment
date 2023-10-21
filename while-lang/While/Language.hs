module While.Language where

import GHC.Natural (Natural)
import Prelude hiding (not)
import qualified Prelude (not)

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
    | And BExp BExp
    | Or  BExp BExp
    | Eq  AExp AExp
    | Neq AExp AExp
    | Low AExp AExp
    | GEq AExp AExp
    deriving (Eq, Show)

not :: BExp -> BExp
not (Lit b) = Lit $ Prelude.not b
not (And b1 b2) = Or (not b1) (not b2)
not (Or b1 b2) = And (not b1) (not b2)
not (Eq e1 e2) = Neq e1 e2
not (Neq e1 e2) = Eq e1 e2
not (Low e1 e2) = GEq e1 e2
not (GEq e1 e2) = Low e1 e2

greaterThan :: AExp -> AExp -> BExp
greaterThan e1 e2 = Low e2 e1

lowerEqual :: AExp -> AExp -> BExp
lowerEqual e1 e2 = GEq e2 e1

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
