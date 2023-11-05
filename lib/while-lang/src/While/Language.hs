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

data BinOpAssoc
    = LeftToRight
    | RightToLeft

data BinOp a = BinOp
    { prec :: Int
    , term :: a -> a -> a
    , assc :: BinOpAssoc
    }

prio :: BinOp a -> Int
prio binop = maxPriorityLevel - prec binop

operMul :: BinOp AExp
operMul = BinOp
    { prec = 5
    , term = Mul
    , assc = LeftToRight
    }

operSum :: BinOp AExp
operSum = BinOp
    { prec = 6
    , term = Sum
    , assc = LeftToRight
    }

operSub :: BinOp AExp
operSub = BinOp
    { prec = 6
    , term = Sub
    , assc = LeftToRight
    }

operDiv :: BinOp AExp
operDiv = BinOp
    { prec = 5
    , term = Div
    , assc = LeftToRight
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
    { prec = 14
    , term = And
    , assc = LeftToRight
    }

operOr :: BinOp BExp
operOr = BinOp
    { prec = 15
    , term = Or
    , assc = LeftToRight
    }

data Stmt
    = Assg String AExp
    | Skip
    | Cons Stmt Stmt
    | Brnc BExp Stmt Stmt
    | Loop BExp Stmt
    deriving (Show, Eq)

operCons :: BinOp Stmt
operCons = BinOp
    { prec = 17
    , term = Cons
    , assc = RightToLeft
    }