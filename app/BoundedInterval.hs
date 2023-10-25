module BoundedInterval where
import Data.Reflection (Reifies (reflect), reify)
import Data.Proxy (Proxy (Proxy))
import Interval
import AI
import Algebra.Lattice

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

