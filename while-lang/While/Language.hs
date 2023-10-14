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
    deriving (Eq, Show)

data BinOp = BinOp
    { prec :: Int
    , term :: AExp -> AExp -> AExp
    }

operMul :: BinOp
operMul = BinOp 
    { prec = 4
    , term = Mul
    }

operSum :: BinOp
operSum = BinOp 
    { prec = 4
    , term = Sum
    }

operSub :: BinOp
operSub = BinOp 
    { prec = 4
    , term = Sub
    }

operDiv :: BinOp
operDiv = BinOp 
    { prec = 4
    , term = Div
    }