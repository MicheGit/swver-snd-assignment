module Main (main) where

import Tests.AExpTests

import System.Exit as Exit

import Test.HUnit

tests :: Test
tests = TestList 
    [ aexprTests
    ]

main = do
    result <- runTestTT tests
    if failures result > 0
        then Exit.exitFailure
        else Exit.exitSuccess