module Main (main) where

import System.Exit as Exit
import Test.HUnit

import Algebra.Lattice
-- import AbstractInterpreter
-- import BoundedIntervalAnalysis

-- input1 = assumeParseString "x := 1"

-- expected1 = top |-> ("x", bind 1)

tests :: Test
tests = TestList
    [
        -- TestCase $ assertEqual expected1 (bindAnalysis (1,1) input1)
    ]

main = do
    result <- runTestTT tests
    if failures result > 0
        then Exit.exitFailure
        else Exit.exitSuccess