module AbstractDomains.Extra where

import Algebra.Lattice
import Algebra.PartialOrd

instance (Eq a, Lattice a) => PartialOrd a where
  leq :: (Eq a, Lattice a) => a -> a -> Bool
  leq a b = a \/ b == b

class (BoundedLattice a) => WidenedLattice a where
    (\\//) :: a -> a -> a

class PartialCmp a where
  minusLow :: a -> a -> a
  minusGrt :: a -> a -> a
  minusLEq :: a -> a -> a
  minusGEq :: a -> a -> a
  forSureLow :: a -> a -> Bool
  forSureGEq :: a -> a -> Bool
  forSureGrt :: a -> a -> Bool
  forSureGrt a1 a2 = forSureLow a2 a1
  forSureLEq :: a -> a -> Bool
  forSureLEq a1 a2 = forSureGEq a2 a1