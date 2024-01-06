module BoundedIntervalAnalysis where

import Prelude hiding (lookup)

import Data.Reflection
import Data.Proxy

import While.Language

import Algebra.PartialOrd ( PartialOrd(leq) )
import Algebra.Lattice

import AbstractDomains.Interval
import AbstractDomains.BoundedInterval
import AbstractDomains.InfiniteIntegers
import AbstractDomains.Extra

import AbstractInterpreter

abstractBinOp op e1 e2 s =
  let (BI a1, s1) = abstractA e1 s
      (BI a2, s2) = abstractA e2 s1
   in (bind $ a1 `op` a2, s2)

instance (r ~ (InfInt, InfInt), Reifies s r) => AI (BoundedInterval s r) where
  widen :: (state ~ AState (BoundedInterval s r)) => state -> state -> state
  widen x succ                                              -- the x_{n+1} value in widened iteration sequences in bounded intervals is:
    | m /= NegInfinity && n /= Infinity   = succ            -- F(x_n) - if the interval domain is bounded, the k-k-t sequence converges (no widen operation is required)
    | succ `leq` x                        = x               -- x_n - if the domain is not bounded, adding no new information forces the widened iteration sequence to converge
    | otherwise                           = x \\// succ     -- widen - if the domain is not bounded, new informations might not converge, therefore the widen operator is applied, simulating an infinite iteration sequence
    where
      (m, n) = reflect (Proxy :: Proxy s)

  abstractA :: (state ~ AState (BoundedInterval s r)) => AExp -> state -> (BoundedInterval s r, state)
  abstractA (Nat n) s =
    let i :: Interval
        i = fromIntegral n
     in (bind i, s)
  abstractA (Var x) s = (lookup x s, s)
  abstractA (Neg e) s =
    let (BI a, s') = abstractA e s
     in (bind $ -a, s')
  abstractA (Sum e1 e2) s = abstractBinOp (+) e1 e2 s
  abstractA (Sub e1 e2) s = abstractBinOp (-) e1 e2 s
  abstractA (Mul e1 e2) s = abstractBinOp (*) e1 e2 s
  abstractA (Div e1 e2) s = abstractBinOp (/) e1 e2 s
  abstractA (Inc x) s =
    let BI val = lookup x s
     in (bind val, s |-> (x, bind $ val + 1))
  abstractA (Dec x) s =
    let BI val = lookup x s
     in (bind val, s |-> (x, bind $ val - 1))
  abstractA (PrefixInc x) s =
    let BI val = lookup x s
        val' = bind $ val + 1
        s' = s |-> (x, val')
     in (val', s')
  abstractA (PrefixDec x) s =
    let BI val = lookup x s
        val' = bind $ val - 1
        s' = s |-> (x, val')
     in (val', s')


  abstractB :: (state ~ AState (BoundedInterval s r)) => BExp -> state -> (state, state)
  abstractB (Lit True) s = (s, bottom)
  abstractB (Lit False) s = (bottom, s)
  abstractB (Or b1 b2) s =
    let (sthen1, selse1) = abstractB b1 s
     in if selse1 == bottom
      then (sthen1, selse1) -- then for sure b1 evaluates to true or a runtime error, must not compute b2
      else let (sthen2, selse2) = abstractB b2 selse1 -- compute only when b1 evaluates to false
            in (sthen1 \/ sthen2, selse2) -- to evaluate false, the only case is that the second one evaluates false
  abstractB (And b1 b2) s =
    let (sthen1, selse1) = abstractB b1 s
     in if sthen1 == bottom
      then (sthen1, selse1) -- then for sure b1 evaluates to false or a runtime error, must not compute b2 
      else let (sthen2, selse2) = abstractB b2 sthen1 -- compute only when b1 evaluates to true
            in (sthen2, selse1 \/ selse2)
  abstractB (Eq e1 e2) s
    | a1 == bottom || a2 == bottom      = (bottom, bottom)
    | a1 /\ a2 == bottom                = (bottom, s2)
    | a1 == a2 && size (unbox a1) == 1  = (s2, bottom)
    | otherwise                         = (applyTransitions (enforceEq s e1 e2), applyTransitions s)
    where
      (a1, s1) = abstractA e1 s
      (a2, s2) = abstractA e2 s1
      applyTransitions = snd . abstractA e2 . snd . abstractA e1
  abstractB (Neq e1 e2) s =
    let (sEq, sNeq) = abstractB (Eq e1 e2) s
     in (sNeq, sEq)
  abstractB (Low e1 e2) s
    | a1 == bottom || a2 == bottom      = (bottom, bottom)
    | forSureLow (unbox a1) (unbox a2)  = (s2, bottom)
    | forSureGEq (unbox a1) (unbox a2)  = (bottom, s2)
    | otherwise                         = (applyTransitions $ enforceLow s e1 e2, applyTransitions $ enforceGEq s e1 e2)
    where
      (a1, s1) = abstractA e1 s
      (a2, s2) = abstractA e2 s1
      applyTransitions = snd . abstractA e2 . snd . abstractA e1
  abstractB (GEq e1 e2) s =
    let (sLow, sGEq) = abstractB (Low e1 e2) s
     in (sGEq, sLow)
  abstractB (Grt e1 e2) s
    | a1 == bottom || a2 == bottom      = (bottom, bottom)
    | forSureGrt (unbox a1) (unbox a2)  = (s2, bottom)
    | forSureLEq (unbox a1) (unbox a2)  = (bottom, s2)
    | otherwise                         = (applyTransitions $ enforceGrt s2 e1 e2, applyTransitions $ enforceLEq s2 e1 e2)
    where
      (a1, s1) = abstractA e1 s
      (a2, s2) = abstractA e2 s1
      applyTransitions = snd . abstractA e2 . snd . abstractA e1
  abstractB (LEq e1 e2) s =
    let (sGrt, sLEq) = abstractB (Grt e1 e2) s
     in (sLEq, sGrt)
{-
Given
- binopLeft and binopRight, two (descending monotone wrt their first parameter) binary operators
- a state s
- two arithmetic expressions e1 e2
then induceCmp will return a state s# s.t.:
- s# <= s
- if e1=Var x, then s#(x) is the gfp of (`binopLeft` e2) starting from s(x)
- if e2=Var y, then s#(y) is the gfp of (`binopRight` e1) starting from s(y)
-}
induceCmp :: AI t => (t -> t -> t) -> (t -> t -> t) -> AState t -> AExp -> AExp -> AState t
induceCmp binopLeft binopRight s e1 e2 = gfpFrom s f
  where
    f s' =
      let (ax, s'') = abstractA e1 s'
          (ay, _  ) = abstractA e2 s''
          refine1 = case e1 of
              (Var x) -> fromList [(x, ax `binopLeft` ay)]
              _ -> top
          refine2 = case e2 of
              (Var y) -> fromList [(y, ay `binopRight` ax)]
              _ -> top
       in s' /\ refine1 /\ refine2

{-
Example 1. Consider s# the result of enforceEq s e1 e2. Then
- s# <= s
- if e1=Var x, then s#(x) is the gfp of (/\ e2) starting from s(x)
- if e2=Var y, then s#(y) is the gfp of (/\ e1) starting from s(y)

Therefore, s#(x) is the smallest value included in s(x) s.t. could equal A#[[e2]] s' (with s' the state after the transition induced by A#[[e1]]).
Similarly, s#(y) is the smallest value included in s(y) s.t. could equal A#[[e1]] s.
-}
enforceEq :: (AI a) => AState a -> AExp -> AExp -> AState a
enforceEq = induceCmp (/\) (/\)

enforceLow :: (AI a, PartialCmp a) => AState a -> AExp -> AExp -> AState a
enforceLow = induceCmp minusGEq minusLEq

enforceGrt :: (AI a, PartialCmp a) => AState a -> AExp -> AExp -> AState a
enforceGrt = induceCmp minusLEq minusGEq

enforceGEq :: (AI a, PartialCmp a) => AState a -> AExp -> AExp -> AState a
enforceGEq = induceCmp minusLow minusGrt

enforceLEq :: (AI a, PartialCmp a) => AState a -> AExp -> AExp -> AState a
enforceLEq = induceCmp minusGrt minusLow

bindAnalysis :: (InfInt, InfInt) -> While -> AState Interval
bindAnalysis bounds program = reify bounds computation
  where
  computation :: forall s. (Boundable s (InfInt, InfInt) Interval) => Proxy s -> AState Interval
  computation _ =
    let result :: AState (BoundedInterval s (InfInt, InfInt))
        result = analyze program
      in AbstractInterpreter.map unbox result

instance (r ~ (InfInt, InfInt), Reifies s r) => LogAI (BoundedInterval s r)

bindAnalysisLog :: (InfInt, InfInt) -> While -> InvariantLog Interval
bindAnalysisLog bounds program = reify bounds computation
  where
    computation :: forall s. (Boundable s (InfInt, InfInt) Interval) => Proxy s -> InvariantLog Interval
    computation _ =
      let result :: [InvariantLog (BoundedInterval s (InfInt, InfInt))]
          result = analyzeLog program
       in decode (AbstractInterpreter.map unbox) (List result)
