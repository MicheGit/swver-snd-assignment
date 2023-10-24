{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
module AI where
import qualified Data.HashMap.Lazy as HM
import Algebra.Lattice
import Data.Maybe (fromMaybe)
import While.Language (Stmt (Assg, Skip, Cons, Brnc, Loop), AExp, BExp, not)

{- The type of the abstract state. 

AState abstracts the powerset \wp(State), with `a` being an abstract representation of the values of the variable.

The bottom element represents failures, which propagate over all the program (in eager evaluation). 
However, the standard bounded lattice implementation of hasmaps handles empty hashmaps as bottom values and, generically, a missing element as bottom. Therefore, we need two distinct elements to model bottom and top elements of the states.

Note that the only way to recover from a bottom state is in the if-then-else branching, with a \/ operation.
-}
data (Eq a, BoundedLattice a) => AState a 
    = AState (HM.HashMap String a)
    | Bot 
    | Top
    deriving (Eq)

instance (Eq a, BoundedLattice a) => Lattice (AState a) where
  (\/) :: (Eq a, BoundedLattice a) => AState a -> AState a -> AState a
  (AState s1) \/ (AState s2) = AState (s1 \/ s2)

  (AState s1) /\ (AState s2) = AState (s1 /\ s2)

instance (Eq a, BoundedLattice a) => BoundedJoinSemiLattice (AState a) where
  bottom = Bot

instance (Eq a, BoundedLattice a) => BoundedMeetSemiLattice (AState a) where
  top = Top


{- State update replaces values in the map -}
update :: (Eq a, BoundedLattice a) => String -> a -> AState a -> AState a
update x v (AState map) = AState (HM.insert x v map)

(|->) :: (Eq a, BoundedLattice a) => AState a -> (String, a) -> AState a
s |-> (k, v) = update k v s

{- Not found variables are interpreted as bottom. -}
lookup :: (Eq a, BoundedLattice a) => String -> AState a -> a
lookup k (AState s) = fromMaybe bottom (HM.lookup k s)

type AbstractA a = AExp -> AState a -> (a, AState a)
type AbstractB a = BExp -> AState a -> AState a
type AbstractD a = Stmt -> AState a -> AState a 

type While = Stmt

class (Eq a, BoundedLattice a) => AI a where
    abstractA :: AExp -> AState a -> (a, AState a)
    abstractB :: BExp -> AState a -> AState a
    abstractD :: Stmt -> AState a -> AState a
    abstractD (Assg x e) s = let (a, s1) = abstractA e s
                              in s1 |-> (x, a)
    abstractD Skip s = s
    abstractD (Cons st1 st2) s = (abstractD st2 . abstractD st1) s
    abstractD (Brnc b st1 st2) s = (abstractD st1 . abstractB b) s \/ (abstractD st2 . abstractB (While.Language.not b)) s
    abstractD (Loop b st) s = abstractB b (lfp ((s \/) . (abstractD st . abstractB b)))
    analyze :: While -> AState a -- \times Log = [String] (opzionale)
    analyze program = abstractD program top









