module Tests.AExpTests where

import Tests.Common

import While.Parser
import While.Language

import Test.HUnit

-- var
testVariableName = TestCase $ assertEqual "should be x" "x" (assumeParseString parseVariable "x")

-- aexpr

testNat = testFactory (Nat 2) (parse "2")
testVar = testFactory (Var "x") (parse "x")
testNeg1 = testFactory (Neg (Nat 2)) (parse "-2")
testNeg2 = testFactory (Neg (Var "x")) (parse "-x")

testInc1 = testFactory (Inc "x") (parse "x++")
testInc2 = testFactory (PrefixInc "x") (parse "++x")
testDec1 = testFactory (Dec "x") (parse "x--")
testDec2 = testFactory (PrefixDec "x") (parse "--x")

testOp1 = testFactory (Mul (Nat 2) (Neg (Nat 3))) (parse "2* -3")
testOp2 = testFactory (Mul (Mul (Neg (Nat 3)) (Neg (Nat 4))) (Neg (Mul (Nat 3) (Nat 0)))) (parse "-3*-4 * -(3 * 0)")
testOp3 = testFactory (Mul (Nat 2) (Neg (PrefixDec "x"))) (parse "2* -(--x)")
testOp4 = testFactory (Mul (Nat 2) (Neg (PrefixDec "x"))) (parse "2*---x")
testOp5 = testFactory (Sum (Var "x") (Mul (Nat 2) (Inc "i"))) (parse "x + 2 * i++")

aexprTests = TestList
    [ testVariableName
    , testNat
    , testVar
    , testNeg1
    , testNeg2
    , testInc1
    , testInc2
    , testDec1
    , testDec2
    , testOp1
    , testOp2
    , testOp3
    , testOp4
    , testOp5
    ]