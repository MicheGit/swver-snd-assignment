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
    | otherwise                         = (enforceEq s2 e1 e2 a1 a2, s2)
    where
      (a1, s1) = abstractA e1 s
      (a2, s2) = abstractA e2 s1
  abstractB (Neq e1 e2) s =
    let (sEq, sNeq) = abstractB (Eq e1 e2) s
     in (sNeq, sEq)
  abstractB (Low e1 e2) s
    | a1 == bottom || a2 == bottom      = (bottom, bottom)
    | forSureLow (unbox a1) (unbox a2)  = (s2, bottom)
    | forSureGEq (unbox a1) (unbox a2)  = (bottom, s2)
    | otherwise                         = (enforceLow s2 e1 e2 a1 a2, enforceGEq s2 e1 e2 a1 a2)
    where
      (a1, s1) = abstractA e1 s
      (a2, s2) = abstractA e2 s1
  abstractB (GEq e1 e2) s =
    let (sLow, sGEq) = abstractB (Low e1 e2) s
     in (sGEq, sLow)
  abstractB (Grt e1 e2) s
    | a1 == bottom || a2 == bottom      = (bottom, bottom)
    | forSureGrt (unbox a1) (unbox a2)  = (s2, bottom)
    | forSureLEq (unbox a1) (unbox a2)  = (bottom, s2)
    | otherwise                         = (enforceGrt s2 e1 e2 a1 a2, enforceLEq s2 e1 e2 a1 a2)
    where
      (a1, s1) = abstractA e1 s
      (a2, s2) = abstractA e2 s1
  abstractB (LEq e1 e2) s =
    let (sGrt, sLEq) = abstractB (Grt e1 e2) s
     in (sLEq, sGrt) 

enforceEq :: (AI (BoundedInterval s (InfInt, InfInt))) => AState (BoundedInterval s (InfInt, InfInt)) -> AExp -> AExp -> BoundedInterval s (InfInt, InfInt) -> BoundedInterval s (InfInt, InfInt) -> AState (BoundedInterval s (InfInt, InfInt))
enforceEq s2 e1 e2 a1 a2 = (snd . abstractA e2 . snd . abstractA e1) (gfpFrom s2 _F)
  where
    _F s = 
      let refine1 = case e1 of
              (Var x) -> fromList [(x, fst $ abstractA e2 s)]
              _ -> top
          refine2 = case e1 of
              (Var y) -> fromList [(y, fst $ abstractA e1 s)]
              _ -> top
       in s /\ refine1 /\ refine2


enforceLow :: (Reifies s (InfInt, InfInt), AI (BoundedInterval s (InfInt, InfInt))) => AState (BoundedInterval s (InfInt, InfInt)) -> AExp -> AExp -> BoundedInterval s (InfInt, InfInt) -> BoundedInterval s (InfInt, InfInt) -> AState (BoundedInterval s (InfInt, InfInt))
enforceLow s2 (Var x) (Var y) (BI a1) (BI a2) = s2 |-> (x, bind $ a1 `minusGEq` a2) |-> (y, bind $ a2 `minusLEq` a1)
enforceLow s2 _ (Var y) (BI a1) (BI a2)       = s2 |-> (y, bind $ a2 `minusLEq` a1)
enforceLow s2 (Var x) e2 (BI a1) _            = (snd . abstractA e2) (gfpFrom s2 _F)
  where
    _F s = s |-> (x, bind $ a1 `minusGEq` unbox (fst (abstractA e2 s)))
enforceLow s2 _ _ _ _ = s2

enforceGrt :: (Reifies s (InfInt, InfInt), AI (BoundedInterval s (InfInt, InfInt))) => AState (BoundedInterval s (InfInt, InfInt)) -> AExp -> AExp -> BoundedInterval s (InfInt, InfInt) -> BoundedInterval s (InfInt, InfInt) -> AState (BoundedInterval s (InfInt, InfInt))
enforceGrt s2 (Var x) (Var y) (BI a1) (BI a2) = s2 |-> (x, bind $ a1 `minusLEq` a2) |-> (y, bind $ a2 `minusGEq` a1)
enforceGrt s2 _ (Var y) (BI a1) (BI a2)       = s2 |-> (y, bind $ a2 `minusGEq` a1)
enforceGrt s2 (Var x) e2 (BI a1) _            = (snd . abstractA e2) (gfpFrom s2 _F)
  where
    _F s = s |-> (x, bind $ a1 `minusLEq` unbox (fst (abstractA e2 s)))
enforceGrt s2 _ _ _ _ = s2

enforceGEq :: (Reifies s (InfInt, InfInt), AI (BoundedInterval s (InfInt, InfInt))) => AState (BoundedInterval s (InfInt, InfInt)) -> AExp -> AExp -> BoundedInterval s (InfInt, InfInt) -> BoundedInterval s (InfInt, InfInt) -> AState (BoundedInterval s (InfInt, InfInt))
enforceGEq s2 (Var x) (Var y) (BI a1) (BI a2) = s2 |-> (x, bind $ a1 `minusLow` a2) |-> (y, bind $ a2 `minusGrt` a1)
enforceGEq s2 _ (Var y) (BI a1) (BI a2)       = s2 |-> (y, bind $ a2 `minusGrt` a1)
enforceGEq s2 (Var x) e2 (BI a1) _            = (snd . abstractA e2) (gfpFrom s2 _F)
  where
    _F s = s |-> (x, bind $ a1 `minusLow` unbox (fst (abstractA e2 s)))
enforceGEq s2 _ _ _ _ = s2

enforceLEq :: Reifies s1 (InfInt, InfInt) => AState (BoundedInterval s1 (InfInt, InfInt)) -> AExp -> AExp -> BoundedInterval s2 r1 -> BoundedInterval s3 r2 -> AState (BoundedInterval s1 (InfInt, InfInt))
enforceLEq s2 (Var x) (Var y) (BI a1) (BI a2) = s2 |-> (x, bind $ a1 `minusGrt` a2) |-> (y, bind $ a2 `minusLow` a1)
enforceLEq s2 _ (Var y) (BI a1) (BI a2)       = s2 |-> (y, bind $ a2 `minusLow` a1)
enforceLEq s2 (Var x) e2 (BI a1) _            = (snd . abstractA e2) (gfpFrom s2 _F)
  where
    _F s = s |-> (x, bind $ a1 `minusGrt` unbox (fst (abstractA e2 s)))
enforceLEq s2 _ _ _ _ = s2

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
