module BoundedInterval where
import Data.Reflection (Reifies (reflect), reify)
import Data.Proxy (Proxy (Proxy))
import Interval
import AI
import Algebra.Lattice
import GHC.Natural (Natural)
import While.Language
import Prelude hiding ((*), (/), (+), (-), negate)
import qualified Prelude

mkBound :: (Reifies s (Rational, Rational)) => Proxy s -> Interval
mkBound bound = uncurry Range (reflect bound)

newtype (r ~ (Rational, Rational), Reifies s r) => BoundedInterval s r = BI Interval
    deriving (Show, Eq)

unbox :: BoundedInterval s r -> Interval
unbox (BI i) = i

{-
A type a is Boundable from a value r reified by the type s.
-}
class Reifies s r => Boundable s r a where
  bind :: a -> BoundedInterval s r

instance (Reifies s (Rational, Rational)) => Boundable s (Rational, Rational) Interval where
  bind :: Interval -> BoundedInterval s (Rational, Rational)
  bind Interval.Bot = bottom
  bind rng =
    let widener = mkBound (Proxy :: Proxy s)
     in BI (rng \\// widener)

instance Lattice (BoundedInterval s r) where
  (BI i) \/ (BI j) = BI $ i \/ j
  (BI i) /\ (BI j) = BI $ i /\ j

instance BoundedMeetSemiLattice (BoundedInterval s r) where
  top = BI top

instance BoundedJoinSemiLattice (BoundedInterval s r) where
  bottom = BI bottom

instance WidenedLattice (BoundedInterval s r) where
  (BI i) \\// (BI j) = BI (i \\// j)

abstractBinOp op e1 e2 s =
  let (BI a1, s1) = abstractA e1 s
      (BI a2, s2) = abstractA e2 s1
   in (bind $ a1 `op` a2, s2)

instance (r ~ (Rational, Rational), Reifies s r) => AI (BoundedInterval s r) where
  abstractA (Nat n) s = (bind $ fromNatural n, s)
  abstractA (Var x) s = (AI.lookup x s, s)
  abstractA (Sum e1 e2) s = abstractBinOp (+) e1 e2 s
  abstractA (Sub e1 e2) s = abstractBinOp (-) e1 e2 s
  abstractA (Mul e1 e2) s = abstractBinOp (*) e1 e2 s
  abstractA (Div e1 e2) s = abstractBinOp (/) e1 e2 s
  abstractA (Inc x) s =
    let BI val = AI.lookup x s
     in (bind val, s |-> (x, bind $ val + fromNatural 1))
  abstractA (Dec x) s =
    let BI val = AI.lookup x s
     in (bind val, s |-> (x, bind $ val - fromNatural 1))
  abstractA (PrefixInc x) s =
    let BI val = AI.lookup x s
        val' = bind $ val + fromNatural 1
        s' = s |-> (x, val')
     in (val', s')
  abstractA (PrefixDec x) s =
    let BI val = AI.lookup x s
        val' = bind $ val - fromNatural 1
        s' = s |-> (x, val')
     in (val', s')

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
  abstractB (Eq e1 e2) s =
    let (a1, s1) = abstractA e1 s
        (a2, s2) = abstractA e2 s1
     in if a1 == bottom || a2 == bottom
      then (bottom, bottom)
      else 
  abstractB (Neq e1 e2) s =
    let (BI a1, s1) = abstractA e1 s
        (BI a2, s2) = abstractA e2 s1
     in if a1 == a2 && size a1 == 1
      then bottom
      else s2
