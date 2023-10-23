{-# LANGUAGE TypeFamilies #-}
module AI where
import qualified Data.HashMap.Lazy as HM
import Algebra.Lattice
import Data.Maybe (fromMaybe)
import While.Language (Stmt)


{- The type of the abstract state. 
Note that if a is a complete lattice then AState a
is a complete lattice as well, and the implementation is 
provided by the library lattices itself.

AState abstracts the powerset \wp(State), with `a` being
an abstract representation of the values of the variable.

-}
type AState a = HM.HashMap String a

{- State update replaces values in the map -}
update :: String -> a -> AState a -> AState a
update = HM.insert

(|->) :: AState a -> (String, a) -> AState a
s |-> (k, v) = update k v s

{- Not found variables are treated as bottom. -}
lookup :: (BoundedJoinSemiLattice a) => String -> AState a -> a
lookup k s = fromMaybe bottom (HM.lookup k s)

type While = Stmt

class (BoundedLattice a) => AI a where
    ai :: While -> AState a -> AState a -- \times Log = [String] (opzionale)
    -- ai = d#
    










