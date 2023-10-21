module Main (main) where

import System.Exit as Exit

import Test.HUnit

import Tests.AExpTests
import Tests.BExpTests

tests :: Test
tests = TestList 
    [ aexprTests
    , bexprTests
    ]

main = do
    result <- runTestTT tests
    if failures result > 0
        then Exit.exitFailure
        else Exit.exitSuccess