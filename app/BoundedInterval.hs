{-# LANGUAGE FlexibleContexts #-}
module BoundedInterval where
import Data.Reflection (Reifies (reflect), reify)
import Data.Proxy (Proxy)
import Interval (Interval(Range))
import AI
import Algebra.Lattice

mkBound :: (Reifies s (Rational, Rational)) => Proxy s -> Interval
mkBound bound = uncurry Range (reflect bound)

newtype BoundedInterval s r = BI Interval
    deriving (Show, Eq)

instance Lattice (BoundedInterval s r) where
  (BI i) \/ (BI j) = BI $ i \/ j
  (BI i) /\ (BI j) = BI $ i /\ j

instance BoundedMeetSemiLattice (BoundedInterval s r) where 
  top = BI top

instance BoundedJoinSemiLattice (BoundedInterval s r) where
  bottom = BI bottom


instance (Reifies s r) => AI (BoundedInterval s r) where
  abstractA = error "Not implemented"
  abstractB = error "Not implemented"


{-
>>>example1
Range (3 % 1) (3 % 1)
-}
example1 :: Interval
example1 = reify (3, 3) mkBound

intervalAnalize :: (Reifies s r) => s -> While -> AState (BoundedInterval s r)
intervalAnalize bound = analyze
