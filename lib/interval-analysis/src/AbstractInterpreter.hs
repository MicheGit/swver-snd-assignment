module AbstractInterpreter where

import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Lazy as HM

import Algebra.Lattice

import AbstractDomains.Extra
import While.Language
import Data.HashMap.Lazy (toList)
import Data.List (intercalate)


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
    deriving (Eq)

fromList :: (Eq a, BoundedLattice a) => [(String, a)] -> AState a
fromList = foldl (|->) top

showVariable :: (Show a1, Show a2) => (a1, a2) -> String
showVariable (k, v) = show k ++ ": " ++ show v

instance (Eq a, BoundedLattice a, Show a) => Show (AState a) where
  show Bot = "BOTTOM STATE"
  show (AState hm) = "{" ++ intercalate "," (showVariable <$> toList hm) ++ "}"

map :: (Eq a, BoundedLattice a, Eq b, BoundedLattice b) => (a -> b) -> AState a -> AState b
map _ Bot = Bot
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
  _ \\// Bot = Bot
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
update _ _ Bot = Bot

(|->) :: (Eq a, BoundedLattice a) => AState a -> (String, a) -> AState a
s |-> (k, v) = update k v s

{- Not found variables are interpreted as top. -}
lookup :: (Eq a, BoundedLattice a) => String -> AState a -> a
lookup k (AState s) = fromMaybe top (HM.lookup k s)
lookup _ Bot = bottom

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
  abstractD (Loop b st) s =
    let widenedInvariant = unsafeLfp (loopIteration _F)
        narrowedInvariant = gfpFrom widenedInvariant _F -- this refines the widened invariant 
        -- cutting out all the states that are not a target state from some state in the invariant.
        -- Since this domain has no infinite descending chains, the gfp always converges 
        -- in finite time, thus there is no use for a narrowing approximation of the glb.
     in snd $ abstractB b narrowedInvariant
    where
      _F = (s \/) . abstractD st . fst . abstractB b
      --                  ^^^
      -- fst takes the states where b can be true

  analyze :: While -> AState a -- \times Log = [String] (opzionale)
  analyze program = abstractD program top

data (Show a) => InvariantLog a
    = Leaf Stmt (AState a)
    | List [InvariantLog a]
    | Node BExp [InvariantLog a] [InvariantLog a]
    | Cycl BExp [InvariantLog a]
    deriving (Eq)  

printStmts :: (Show a) => [a] -> String
printStmts = intercalate "\n" . (show <$>)

instance (Eq a, BoundedLattice a, Show a) => Show (InvariantLog a) where
  show (Leaf st state) = show st ++ "; // " ++ show state
  show (List xs) = printStmts xs
  show (Node b xs ys) = "if " ++ show b ++ " then {\n" ++ printStmts xs ++ "\n} else {\n" ++ printStmts ys ++ "\n};"
  show (Cycl b xs) = "while " ++ show b ++ " do {\n" ++ printStmts xs ++ "\n};"
      

decode :: (Show a, Show b) => (AState a -> AState b) -> InvariantLog a -> InvariantLog b
decode f (Leaf s a) = Leaf s (f a)
decode f (List xs) = List (decode f <$> xs)
decode f (Node b xs ys) = Node b (decode f <$> xs) (decode f <$> ys)
decode f (Cycl b xs) = Cycl b (decode f <$> xs)

class (AI a, Show a) => LogAI a where
  abstractDLog :: While -> (AState a, [InvariantLog a]) -> (AState a, [InvariantLog a])
  abstractDLog (Assg x e) (s, log) =
    let s' = abstractD (Assg x e) s
      in (s', log ++ [Leaf (Assg x e) s'])
  abstractDLog Skip (s, log) = (s, log ++ [Leaf Skip s])
  abstractDLog (Cons st1 st2) (s, log) = (abstractDLog st2 . abstractDLog st1) (s, log)
  abstractDLog (Brnc b st1 st2) (s, log) =
    let (sTrue, sFalse) = abstractB b s
        (sThen, logThen) = abstractDLog st1 (sTrue, [Leaf Skip sTrue])
        (sElse, logElse) = abstractDLog st2 (sFalse, [Leaf Skip sFalse])
        sAfter = sThen \/ sElse
      in (sAfter, log ++ [Node b logThen logElse, Leaf Skip sAfter])
  abstractDLog (Loop b st) (s, log) =
      let widenedInvariant = unsafeLfp (loopIteration _F)
          narrowedInvariant = gfpFrom widenedInvariant _F -- this refines the widened invariant 
          -- cutting out all the states that are not a target state from some state in the invariant.
          -- Since this domain has no infinite descending chains, the gfp always converges 
          -- in finite time, thus there is no use for a narrowing approximation of the glb.
          (enterLoop, afterLoop) = abstractB b narrowedInvariant
          (_, logFromInvariant) = abstractDLog st (enterLoop, []) -- the log starting from the invariant (should end into the invariant as well)
      in (afterLoop, log ++ [Leaf Skip narrowedInvariant, Cycl b (Leaf Skip enterLoop:logFromInvariant), Leaf Skip afterLoop])
      where
        _F = (s \/) . abstractD st . fst . abstractB b

  analyzeLog :: While -> [InvariantLog a]
  analyzeLog program = snd $ abstractDLog program (top, [])

{-
Represents a (possibly widened) iteration sequence with:
- F(x) = (initialState \/ (D#[[S]] . B#[[b]]) x )
- x^{n+1} = x^n `widen` F(x^n)
When the AI class is istantiated with (m,n) as bounds, then
  widen x^n succ = succ, so:
    x^{n+1} = x^n `widen` F(x^n) = F(x^n) 
  as in a simple fixpoint iteration sequence.
When, instead, one of the bounds is infinite, then
  widen x^n succ = x^n \\// succ, so:
    x^{n+1} = x^n \\// F(x^n)
  as in a widened iteration sequence.

-}
loopIteration :: (AI a) => (AState a -> AState a) -> AState a -> AState a
loopIteration _F precondition = precondition `widen` _F precondition

