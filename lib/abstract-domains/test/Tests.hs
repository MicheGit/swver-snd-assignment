{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
module Main (main) where
import Test.HUnit (runTestTT, Test (TestList, TestCase), Counts (failures), assertEqual, assertBool)
import qualified System.Exit as Exit
import AbstractDomains.InfiniteIntegers (InfInt(Infinity, NegInfinity))
import AbstractDomains.Interval (Interval(Range))
import Data.Reflection (reify, Reifies)
import AbstractDomains.BoundedInterval (unbox, Boundable (bind), BoundedInterval (BI))
import Data.Proxy (Proxy(Proxy))

bindInf :: Interval -> Interval
bindInf i = reify (-Infinity, Infinity) computation
    where
        computation :: forall s. (Boundable s (InfInt, InfInt) Interval) => Proxy s -> Interval
        computation reifiedBounds =
            let j :: BoundedInterval s (InfInt, InfInt)
                j = bind i
            in unbox j


tests = TestList
    [ TestCase $ assertEqual "should be almost 2" (Range 2 Infinity) (1 + Range 1 Infinity)
    , TestCase $ assertEqual "should be almost 2" (Range 2 Infinity) (Range 1 Infinity + 1)
    , TestCase $ assertEqual "should be almost 2" (Range 2 Infinity) (bindInf (bindInf $ Range 1 Infinity + bindInf 1))
    , TestCase $ assertBool "should be correctly ordered" (Infinity > -Infinity)
    ]

main = do
    result <- runTestTT tests
    if failures result > 0
        then Exit.exitFailure
        else Exit.exitSuccess