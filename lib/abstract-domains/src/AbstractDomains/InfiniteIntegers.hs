module AbstractDomains.InfiniteIntegers where

import GHC.Natural (Natural)
import GHC.Real (infinity)

import Algebra.Lattice

data InfInt
    = Finite Integer
    | Infinity
    | NegInfinity
    deriving (Eq, Show)

isFinite :: InfInt -> Bool
isFinite (Finite _) = True
isFinite _          = False

instance Num InfInt where  
  (+) :: InfInt -> InfInt -> InfInt
  Finite i + Finite j     = Finite $ i + j
  Infinity + NegInfinity  = error "Infinity - Infinity"
  Infinity + _            = Infinity
  _ + NegInfinity         = NegInfinity
  a + b                   = b + a

  (*) :: InfInt -> InfInt -> InfInt
  Finite 0 * _            = Finite 0
  _ * Finite 0            = Finite 0
  Finite i * Finite j     = Finite $ i * j
  a * b
    | signum a == signum b  = Infinity
    | otherwise             = NegInfinity

  abs :: InfInt -> InfInt
  abs (Finite i)  = Finite (abs i)
  abs Infinity    = Infinity
  abs NegInfinity = Infinity

  signum :: InfInt -> InfInt
  signum (Finite i)   = Finite (signum i)
  signum Infinity     = Finite 1
  signum NegInfinity  = Finite (-1)

  fromInteger :: Integer -> InfInt
  fromInteger = Finite

  negate :: InfInt -> InfInt
  negate (Finite i)   = Finite (negate i)
  negate Infinity     = NegInfinity
  negate NegInfinity  = Infinity

instance Ord InfInt where
  (<=) :: InfInt -> InfInt -> Bool
  Finite i <= Finite j  = i <= j
  _ <= Infinity         = True
  NegInfinity <= _      = True
  _ <= _                = False

instance Enum InfInt where
  toEnum :: Int -> InfInt
  toEnum = Finite . fromIntegral

  fromEnum :: InfInt -> Int
  fromEnum (Finite i) = fromInteger i
  fromEnum i = error $ "fromEnum applied to" ++ show i

instance Real InfInt where
  toRational :: InfInt -> Rational
  toRational (Finite i) = toRational i
  toRational Infinity = 1/0
  toRational NegInfinity = -1/0

instance Integral InfInt where
  quotRem :: InfInt -> InfInt -> (InfInt, InfInt)
  quotRem (Finite i) (Finite j) =
    let (q, r) = quotRem i j
     in (Finite q, Finite r)
  quotRem (Finite a) b = (Finite 0, Finite a)
  quotRem a (Finite b)
    | b > 0     = (a, a)
    | b < 0     = (-a, a)
    | otherwise = error $ "Divided " ++ show a ++ " over " ++ show b
  quotRem a b = error $ "Divided " ++ show a ++ " over " ++ show b

  toInteger :: InfInt -> Integer
  toInteger (Finite i) = i
  toInteger Infinity = error "toInteger applied to Infinity"
  toInteger NegInfinity = error "toInteger applied to NegInfinity"

instance Lattice Integer where
  (\/) :: Integer -> Integer -> Integer
  (\/) = min
  (/\) :: Integer -> Integer -> Integer
  (/\) = max

instance Lattice InfInt where
  (\/) :: InfInt -> InfInt -> InfInt
  (\/) = min
  (/\) :: InfInt -> InfInt -> InfInt
  (/\) = max

instance BoundedJoinSemiLattice InfInt where
  bottom :: InfInt
  bottom = NegInfinity

instance BoundedMeetSemiLattice InfInt where
  top :: InfInt
  top = Infinity
