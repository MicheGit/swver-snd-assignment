{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import System.Exit as Exit
import Test.HUnit

import While.Parser
import While.Language
import AbstractInterpreter
import BoundedIntervalAnalysis
import AbstractDomains.Interval
import Algebra.Lattice
import AbstractDomains.InfiniteIntegers
import AbstractDomains.BoundedInterval
import Data.Reflection
import Data.Proxy (Proxy(Proxy))

assumeParseString :: Parser a -> String -> a
assumeParseString p text = case parseString p text of
    Left e          -> error (show e)
    Right parsed    -> parsed

parse :: (Parsable a) => String -> a
parse = assumeParseString parseTerm

tuplemap fn (a, b) = (fn a, fn b)

input1 :: While
input1 = parse "x := 1"

expected1 :: AState Interval
expected1 = top |-> ("x", 1)

bindAbstractB :: (InfInt, InfInt) -> BExp -> AState Interval -> (AState Interval, AState Interval)
bindAbstractB bounds b s = reify bounds computation
    where
        computation :: forall s. (Boundable s (InfInt, InfInt) Interval) => Proxy s -> (AState Interval, AState Interval) 
        computation reifiedBounds =
            let result :: (AState (BoundedInterval s (InfInt, InfInt)), AState (BoundedInterval s (InfInt, InfInt))) 
                result = abstractB b (AbstractInterpreter.map bind s)
             in tuplemap (AbstractInterpreter.map unbox) result

bindAbstractA :: (InfInt, InfInt) -> AExp -> AState Interval -> (Interval, AState Interval)
bindAbstractA bounds e s = reify bounds computation
    where
        computation :: forall s. (Boundable s (InfInt, InfInt) Interval) => Proxy s -> (Interval, AState Interval) 
        computation reifiedBounds =
            let result :: (BoundedInterval s (InfInt, InfInt), AState (BoundedInterval s (InfInt, InfInt))) 
                result = abstractA e (AbstractInterpreter.map bind s)
                (BI a, s1) = result
             in (a, AbstractInterpreter.map unbox s1)

test2 :: Test
test2 =
    let expected = fromList [("x", Range 2 Infinity)]
     in TestCase $ assertEqual "x should be 2 minimum" (expected, expected) (bindAbstractB top (parse "x++ < 100") (fromList [("x", Range 1 Infinity)]))

test3 :: Test
test3 = TestCase $ assertEqual "should be 2 minimum, but returning 1 minimum" (Range 1 Infinity, fromList [("x", Range 2 Infinity)]) (bindAbstractA top (Inc "x") (fromList [("x", Range 1 Infinity)]))

test4 :: Test
test4 = TestCase $ assertEqual "should be the same stored" i (AbstractInterpreter.lookup "x" $ top |-> ("x", i))
    where
        i = Range 1 Infinity

test5 :: Test
test5 = TestCase $ assertEqual "should simulate abstractA Inc" (Range 1 Infinity, fromList [("x", Range 2 Infinity)]) (simulateInc "x" (fromList [("x", Range 1 Infinity)]))
    where
        simulateInc :: String -> AState Interval -> (Interval, AState Interval)
        simulateInc var s = reify (-Infinity, Infinity) computation
            where
                computation :: forall s. (Boundable s (InfInt, InfInt) Interval) => Proxy s -> (Interval, AState Interval) 
                computation reifiedBounds =
                    let s' = AbstractInterpreter.map bind s
                        BI val = AbstractInterpreter.lookup var s'
                        s1 :: AState (BoundedInterval s (InfInt, InfInt))
                        s1 = s' |-> (var, bind $ val + 1)
                    in (val, AbstractInterpreter.map unbox s1)

test6 :: Test
test6 = TestCase $ assertEqual "should keep the same range" expected (boxUnbox expected)
    where
        expected = top |-> ("x", Range 1 Infinity)
        boxUnbox :: AState Interval -> AState Interval
        boxUnbox s = reify (-Infinity, Infinity) computation
            where
                computation :: forall s. (Boundable s (InfInt, InfInt) Interval) => Proxy s -> AState Interval
                computation reifiedBounds =
                    let s' :: AState (BoundedInterval s (InfInt, InfInt))
                        s' = AbstractInterpreter.map bind s
                     in AbstractInterpreter.map unbox s'

test7 :: Test
test7 = TestCase $ assertEqual "should not collapse bounds" expected (boxUnbox expected)
    where
        expected = Range 1 Infinity
        boxUnbox :: Interval -> Interval
        boxUnbox i = reify (-Infinity, Infinity) computation
            where
                computation :: forall s. (Boundable s (InfInt, InfInt) Interval) => Proxy s -> Interval
                computation reifiedBounds = 
                    let i' :: BoundedInterval s (InfInt, InfInt)
                        i' = bind i
                     in unbox i'

test8 :: Test
test8 = TestCase $ assertEqual "oh oh" (-Infinity) (reify (-Infinity, Infinity) cmp)
    where
        cmp :: (Boundable s (InfInt, InfInt) Interval) => Proxy s -> InfInt
        cmp reifiedBounds =
            let (d, u) = reflect reifiedBounds
             in d 


tests :: Test
tests = TestList
    [ TestCase $ assertEqual "should be constant 1" expected1 (bindAnalysis (1,1) input1)
    , test2
    , test3
    , test4
    , test5
    , test6
    , test7
    , test8
    ]


main = do
    result <- runTestTT tests
    if failures result > 0
        then Exit.exitFailure
        else Exit.exitSuccess