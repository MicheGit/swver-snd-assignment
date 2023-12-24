module BoundedIntervalAnalysis where

import Prelude hiding (lookup)

import Data.Reflection
import Data.Proxy

import While.Language

import Algebra.PartialOrd
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
    | otherwise                         = (s2, s2)
    where
      (a1, s1) = abstractA e1 s
      (a2, s2) = abstractA e2 s1
  abstractB (Neq e1 e2) s
    | a1 == bottom || a2 == bottom      = (bottom, bottom)
    | a1 /\ a2 == bottom                = (s2, bottom)
    | a1 == a2 && size (unbox a1) == 1  = (bottom, s2)
    | otherwise                         = (s2, s2)
    where
      (a1, s1) = abstractA e1 s
      (a2, s2) = abstractA e2 s1
  abstractB (Low e1 e2) s
    | a1 == bottom || a2 == bottom      = (bottom, bottom)
    | forSureLow a1 a2                  = (s2, bottom)
    | forSureGEq a1 a2                  = (bottom, s2)
    | otherwise                         = (s2, s2)
    where
      (a1, s1) = abstractA e1 s
      (a2, s2) = abstractA e2 s1
  abstractB (GEq e1 e2) s
    | a1 == bottom || a2 == bottom      = (bottom, bottom)
    | forSureLow a1 a2                  = (bottom, s2)
    | forSureGEq a1 a2                  = (s2, bottom)
    | otherwise                         = (s2, s2)
    where
      (a1, s1) = abstractA e1 s
      (a2, s2) = abstractA e2 s1

forSureLow :: BoundedInterval s1 r1 -> BoundedInterval s2 r2 -> Bool
forSureLow (BI (Range _ h)) (BI (Range l _)) = h < l
forSureGEq :: BoundedInterval s1 r1 -> BoundedInterval s2 r2 -> Bool
forSureGEq (BI (Range l _)) (BI (Range _ h)) = l >= h

{-
>>>forSureGEq (BI $ Range (Finite 1) Infinity) (BI $ Range (Finite 100) (Finite 100))
False
-}

bindAnalysis :: (InfInt, InfInt) -> While -> AState Interval
bindAnalysis bounds program = reify bounds computation
  where
  computation :: forall s. (Boundable s (InfInt, InfInt) Interval) => Proxy s -> AState Interval
  computation reifiedBounds =
    let result :: AState (BoundedInterval s (InfInt, InfInt))
        result = analyze program
      in AbstractInterpreter.map unbox result
