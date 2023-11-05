module AbstractInterpreter where

import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Lazy as HM

import Algebra.Lattice 

import AbstractDomains.Extra
import While.Language


{- The type of the abstract state. 

AState abstracts the powerset \wp(State), with `a` being an abstract representation of the values of the variable.

The bottom element represents failures, which propagate over all the program (in eager evaluation, they remain "silent" in lazy evaluation). 
However, the standard bounded lattice implementation of hasmaps handles empty hashmaps as bottom values and, generically, a missing element as bottom. Therefore, we need to "invert" this interpretation.

Note that the only way to recover from a bottom state is in the if-then-else branching, with a \/ operation.

Note also that this state representation works under the assumption that all referenced variables in a program are defined, i.e. their value, if not specified, is any (unknown). This is due to the fact that this analyzer's purpose is not to spot unused variables, but to approximate their values at runtime.  
-}
data (Eq a, BoundedLattice a) => AState a
    = AState (HM.HashMap String a)
    | Bot
    deriving (Eq, Show)

fromList :: (Eq a, BoundedLattice a) => [(String, a)] -> AState a
fromList = foldl (|->) top 

map :: (Eq a, BoundedLattice a, Eq b, BoundedLattice b) => (a -> b) -> AState a -> AState b
map f Bot = Bot
map f (AState hm) = AState (f <$> hm)

instance (Eq a, BoundedLattice a) => Lattice (AState a) where
  (\/) :: (Eq a, BoundedLattice a) => AState a -> AState a -> AState a
  Bot \/ a = a
  a \/ Bot = a
  (AState s1) \/ (AState s2) = AState (HM.intersectionWith (\/) s1 s2)
  -- when a variable is missing in one of the two states, it becomes top
  -- otherwise it becomes the lub of the two occurrences

  (/\) :: (Eq a, BoundedLattice a) => AState a -> AState a -> AState a
  Bot /\ _ = Bot
  _ /\ Bot = Bot
  (AState s1) /\ (AState s2) = AState (HM.unionWith (/\) s1 s2)
  -- when a variable is missing in one of the two states, it is "narrowed" to the other occurence
  -- otherwise it becomes the glb of the two occurrences

instance (Eq a, BoundedLattice a) => BoundedJoinSemiLattice (AState a) where
  bottom :: (Eq a, BoundedLattice a) => AState a
  bottom = Bot

instance (Eq a, BoundedLattice a) => BoundedMeetSemiLattice (AState a) where
  top :: (Eq a, BoundedLattice a) => AState a
  top = AState HM.empty

instance (Eq a, WidenedLattice a) => WidenedLattice (AState a) where
  (\\//) :: (Eq a, WidenedLattice a) => AState a -> AState a -> AState a
  Bot \\// a = a
  a \\// Bot = Bot
  AState s1 \\// AState s2 = AState (HM.intersectionWith (\\//) s1 s2)

{- 
State update replaces values in the map.

The bottom state (an error has occurred) doesn't allow updates.
-}
update :: (Eq a, BoundedLattice a) => String -> a -> AState a -> AState a
update x v (AState map) =
  if v == top
    then AState (HM.delete x map)
    else AState (HM.insert x v map)
update x v Bot = Bot

(|->) :: (Eq a, BoundedLattice a) => AState a -> (String, a) -> AState a
s |-> (k, v) = update k v s

{- Not found variables are interpreted as top. -}
lookup :: (Eq a, BoundedLattice a) => String -> AState a -> a
lookup k (AState s) = fromMaybe top (HM.lookup k s)
lookup k Bot = bottom

type While = Stmt

-- TODO
-- data EvaluationStrategy
--   = Lazy
--   | Eager
--   deriving (Show, Eq)

class (Eq a, BoundedLattice a) => AI a where
  widen :: AState a -> AState a -> AState a
  abstractA :: AExp -> AState a -> (a, AState a)
  -- B# toglie gli stati in cui non può essere vero BExp, e poi gli applica il side effect
  abstractB :: BExp -> AState a -> (AState a, AState a) -- il primo sono gli stati in cui la condizione può essere vera, a cui viene applicato il side effect il secondo sono gli stati in cui la condizione può essere falsa, a cui viene applicato il side effect
  abstractD :: Stmt -> AState a -> AState a
  abstractD (Assg x e) s = let (a, s1) = abstractA e s
                            in if a == bottom -- && evaluationStrategy == Eager 
                              then bottom     -- TODO evaluation strategy: this is eager
                              else s1 |-> (x, a)
  abstractD Skip s = s
  abstractD (Cons st1 st2) s = (abstractD st2 . abstractD st1) s
  abstractD (Brnc b st1 st2) s =
    let (sthen, selse) = abstractB b s
     in abstractD st1 sthen \/ abstractD st2 selse
  abstractD (Loop b st) s = snd $ abstractB b $ lfp (loopIteration b st s)

  analyze :: While -> AState a -- \times Log = [String] (opzionale)
  analyze program = abstractD program top


loopIteration :: (AI a) => BExp -> While -> AState a -> AState a -> AState a
loopIteration b st s precondition =
  let (afterGuardTrue, _) = abstractB b precondition
      postcondition = abstractD st afterGuardTrue
    in widen precondition (s \/ postcondition)

