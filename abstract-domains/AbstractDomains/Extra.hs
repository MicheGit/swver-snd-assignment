module AbstractDomains.Extra where

import Algebra.Lattice
import Algebra.PartialOrd

instance (Eq a, Lattice a) => PartialOrd a where
  leq :: (Eq a, Lattice a) => a -> a -> Bool
  leq a b = a \/ b == b

class (BoundedLattice a) => WidenedLattice a where
    (\\//) :: a -> a -> a