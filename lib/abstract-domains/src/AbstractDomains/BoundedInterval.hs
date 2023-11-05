module AbstractDomains.BoundedInterval where

import Data.Reflection (Reifies (reflect), reify)
import Data.Proxy (Proxy (Proxy))

import Algebra.Lattice

import AbstractDomains.Interval
import AbstractDomains.InfiniteIntegers
import AbstractDomains.Extra


newtype (r ~ (InfInt, InfInt), Reifies s r) => BoundedInterval s r = BI Interval
    deriving (Show, Eq)

unbox :: BoundedInterval s r -> Interval
unbox (BI i) = i


{-
A type a is Boundable from a value r reified by the type s.
-}
class Reifies s r => Boundable s r a where
  bind :: a -> BoundedInterval s r

instance (Reifies s (InfInt, InfInt)) => Boundable s (InfInt, InfInt) Interval where
  bind :: Interval -> BoundedInterval s (InfInt, InfInt)
  bind Bot = bottom
  bind (Range l h) 
    | l == h      = BI (Range l h)
    | d > u       = top
    | otherwise   = BI (Range l1 h1)
    where 
      (d, u) = reflect (Proxy :: Proxy s)
      l1
        | l < d     = -Infinity   
        | l > u     = u
        | otherwise = l
      h1
        | h > u     = Infinity
        | h < d     = d
        | otherwise = h

instance Lattice (BoundedInterval s r) where
  (\/) :: BoundedInterval s r -> BoundedInterval s r -> BoundedInterval s r
  (BI i) \/ (BI j) = BI $ i \/ j
  (/\) :: BoundedInterval s r -> BoundedInterval s r -> BoundedInterval s r
  (BI i) /\ (BI j) = BI $ i /\ j

instance BoundedMeetSemiLattice (BoundedInterval s r) where
  top :: BoundedInterval s r
  top = BI top

instance BoundedJoinSemiLattice (BoundedInterval s r) where
  bottom :: BoundedInterval s r
  bottom = BI bottom

instance WidenedLattice (BoundedInterval s r) where
  (\\//) :: BoundedInterval s r -> BoundedInterval s r -> BoundedInterval s r
  (BI i) \\// (BI j) = BI (i \\// j)
