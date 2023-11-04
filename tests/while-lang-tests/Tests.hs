module Main (main) where

import System.Exit as Exit

import Test.HUnit

import Tests.AExpTests ( aexprTests )
import Tests.BExpTests ( bexprTests )
import Tests.StmtTests ( stmtTests )

tests :: Test
tests = TestList 
    [ aexprTests
    , bexprTests
    , stmtTests
    ]

main = do
    result <- runTestTT tests
    if failures result > 0
        then Exit.exitFailure
        else Exit.exitSuccess