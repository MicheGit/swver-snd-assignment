module Tests.BExpTests where

import Tests.Common ( testFactory, parse )
import While.Language ( BExp(Low, Lit, Eq, Neq, Or, And, GEq) )
import Test.HUnit (Test(TestList))


testTrue = testFactory (Lit True) (parse "true")
testFalse = testFactory (Lit True) (parse "true")

testCmp = TestList $ uncurry testFactory <$>
    [ (Low (parse "2") (parse "3"), parse "2 < 3")
    , (Eq (parse "2 * 3") (parse "x++"), parse "2 * 3 = x++")
    , (Neq (parse "x + ++y") (parse "5 - (-7)"), parse "x + ++y != 5 - (-7)")
    ]

testOps = TestList $ uncurry testFactory <$>
    [ (Or (And (Low (parse "2") (parse "3")) (GEq (parse "6") (parse "++x")))  (Low (parse "y * x") (parse "4")), parse "2 < 3 and not 6 < ++x or 4 > y*x")
    , (parse "3 >= n or x++ != n", parse "not (n > 3 and x++ = n)")
    ]

bexprTests = TestList
    [ testTrue
    , testFalse
    , testCmp
    , testOps
    ]