module AbstractDomains.Interval where

import Algebra.Lattice

import AbstractDomains.InfiniteIntegers
import AbstractDomains.Extra

-- Ranges are between Rationals since we need comparison with Infinity. However, this abstract domain will expect integers only
data Interval
    = Range InfInt InfInt
    | Bot
    deriving (Eq)
  
instance Show Interval where
  show Bot = "[]"
  show (Range a b) = "[" ++ show a ++ ", " ++ show b ++ "]"

binop :: (InfInt -> InfInt -> InfInt) -> Interval -> Interval -> Interval
binop _ Bot _ = Bot
binop _ _ Bot = Bot
binop op (Range l1 h1) (Range l2 h2) =
    let extremes = [x `op` y | x <- [l1, h1], y <- [l2, h2]]
        l3 = minimum extremes
        h3 = maximum extremes
     in Range l3 h3

instance Num Interval where
  (+) :: Interval -> Interval -> Interval
  (+) = binop (+)

  (*) :: Interval -> Interval -> Interval
  (*) = binop (*)

  abs :: Interval -> Interval
  abs Bot = Bot

  abs (Range l h)
    | l < 0 && h >= 0 = Range 0 (max (abs l) h)
    | l < 0 && h < 0  = Range (abs h) (abs l)
    | otherwise       = Range l h
  
  signum :: Interval -> Interval
  signum Bot = Bot
  signum (Range l h) = Range (signum l) (signum h)

  fromInteger :: Integer -> Interval
  fromInteger i = Range (fromInteger i) (fromInteger i)

  negate :: Interval -> Interval
  negate Bot = Bot
  negate (Range l h) = Range (-h) (-l)

instance Enum Interval where
  toEnum :: Int -> Interval
  toEnum = fromIntegral
  fromEnum :: Interval -> Int
  fromEnum i = error $ "Used fromEnum over " ++ show i

instance Fractional Interval where
  fromRational :: Rational -> Interval
  fromRational = fromInteger . round
  
  (/) :: Interval -> Interval -> Interval
  Range _ _ / Range 0 0 = Bot
  a / Range l2 h2 | l2 < 0 && h2 > 0 = binop div a (Range l2 (-1)) \/ binop div a (Range 1 h2)
  a / b = binop div a b

random :: InfInt -> InfInt -> Interval
random l h
  | l > h     = bottom
  | otherwise = Range l h

instance Lattice Interval where
    (\/) :: Interval -> Interval -> Interval
    Bot \/ a = a
    a \/ Bot = a
    Range l1 h1 \/ Range l2 h2 = Range (min l1 l2) (max h1 h2)

    (/\) :: Interval -> Interval -> Interval
    Bot /\ _ = Bot
    _ /\ Bot = Bot
    Range l1 h1 /\ Range l2 h2 =
        let lo = max l1 l2
            hi = min h1 h2
         in if lo > hi then Bot else Range lo hi


instance BoundedJoinSemiLattice Interval where
    bottom :: Interval
    bottom = Bot

instance BoundedMeetSemiLattice Interval where
    top :: Interval
    top = Range (-Infinity) Infinity

size :: Interval -> InfInt
size Bot         = 0
size (Range l h) = h - l

instance WidenedLattice Interval where
  (\\//) :: Interval -> Interval -> Interval
  Bot \\// a = a
  a \\// Bot = a
  (Range l1 h1) \\// (Range l2 h2) = Range
      (if l1 <= l2 then l1 else -Infinity)
      (if h1 >= h2 then h1 else Infinity)

instance PartialCmp Interval where
  -- excludes the values in first argument that are for sure lower than the second argument
  minusLow :: Interval -> Interval -> Interval
  minusLow (Range l1 h1) (Range l2 _) = Range (max l1 l2) h1
  minusLow _ _ = bottom

  minusGrt :: Interval -> Interval -> Interval
  minusGrt (Range l1 h1) (Range _ h2) = Range l1 (min h1 h2)
  minusGrt _ _ = bottom

  minusLEq :: Interval -> Interval -> Interval
  minusLEq (Range l1 h1) (Range l2 _) = Range (max l1 (l2 + 1)) h1
  minusLEq _ _ = bottom

  minusGEq :: Interval -> Interval -> Interval
  minusGEq (Range l1 h1) (Range _ h2) = Range l1 (min h1 (h2 - 1))
  minusGEq _ _ = bottom

  forSureLow :: Interval -> Interval -> Bool
  forSureLow (Range _ h) (Range l _) = h < l
  forSureLow _ _ = False

  forSureGEq :: Interval -> Interval -> Bool
  forSureGEq (Range l _) (Range _ h) = l >= h
  forSureGEq _ _ = False