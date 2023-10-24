{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Interval where

import Algebra.Lattice
import GHC.Real (infinity)
import Prelude hiding ((*), (/), (+), (-), negate)
import qualified Prelude
import AI (AState, AbstractA, (|->), lookup, analyze, While, AbstractB, AI)
import While.Language (AExp (Nat, Var, Neg, Sum, Sub, Mul, Div, Inc, Dec), BExp (Lit))
import Data.Maybe (fromMaybe)
import GHC.Natural (Natural)
import Data.Reflection (Reifies (reflect))
import Data.Data (Proxy (Proxy))

-- Ranges are between Rationals since we need comparison with
-- Infinity. However, this abstract domain will expect integers
-- only
data Interval
    = Range Rational Rational
    | Bot
    deriving (Eq, Show)

widen :: Interval -> Interval -> Interval
widen Bot a = a
widen a Bot = a
widen (Range l1 h1) (Range l2 h2) = Range
    (if l1 < l2 then l1 else -infinity)
    (if h1 > h2 then h1 else infinity)

(\\//) :: Interval -> Interval -> Interval
(\\//) = widen

instance Lattice Interval where
    Bot \/ a = a
    a \/ Bot = a
    Range l1 h1 \/ Range l2 h2 = Range (min l1 l2) (max h1 h2)

    Bot /\ _ = Bot
    _ /\ Bot = Bot
    Range l1 h1 /\ Range l2 h2 =
        let lo = max l1 l2
            hi = min h1 h2
         in if lo > hi then Bot else Range lo hi


instance BoundedJoinSemiLattice Interval where
    bottom :: Interval
    bottom = Bot

instance BoundedMeetSemiLattice Interval where
    top :: Interval
    top = Range (-infinity) infinity

binop :: (Rational -> Rational -> Rational) -> Interval -> Interval -> Interval
binop _ Bot _ = Bot
binop _ _ Bot = Bot
binop op (Range l1 h1) (Range l2 h2) =
    let extremes = [x `op` y | x <- [l1, h1], y <- [l2, h2]]
        l3 = minimum extremes
        h3 = maximum extremes
     in Range l3 h3

(+) :: Interval -> Interval -> Interval
(+) = binop (Prelude.+)

(-) :: Interval -> Interval -> Interval
(-) = binop (Prelude.-)

(*) :: Interval -> Interval -> Interval
(*) = binop (Prelude.*)

(/) :: Interval -> Interval -> Interval
Range _ _ / Range 0 0 = Bot
a / Range l2 h2 | l2 < 0 && h2 > 0 = binop (Prelude./) a (Range l2 (-1)) \/ binop (Prelude./) a (Range 1 h2)
a / b = binop (Prelude./) a b

negate :: Interval -> Interval
negate Bot = Bot
negate (Range l h) = Range (-h) (-l)

fromNatural :: Natural -> Interval
fromNatural n =
    let r = toRational n
     in Range r r
