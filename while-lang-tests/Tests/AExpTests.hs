module Tests.AExpTests where

import While.Parser
import While.Language

import Test.HUnit

assumeParseString :: Parser a -> String -> a
assumeParseString p text = case parseString p text of
    Left e          -> error (show e)
    Right parsed    -> parsed

-- var
testVariableName = TestCase $ assertEqual "should be x" "x" (assumeParseString parseVariable "x")

testFactory expected = TestCase . assertEqual ("should be " ++ show expected) expected

-- aexpr
pa = assumeParseString parseAExp

testNat = testFactory (Nat 2) (pa "2")
testVar = testFactory (Var "x") (pa "x")
testNeg1 = testFactory (Neg (Nat 2)) (pa "-2")
testNeg2 = testFactory (Neg (Var "x")) (pa "-x")

testInc1 = testFactory (Inc "x") (pa "x++")
testInc2 = testFactory (PrefixInc "x") (pa "++x")
testDec1 = testFactory (Dec "x") (pa "x--")
testDec2 = testFactory (PrefixDec "x") (pa "--x")

testOp1 = testFactory (Mul (Nat 2) (Neg (Nat 3))) (pa "2* -3")
testOp2 = testFactory (Mul (Mul (Neg (Nat 3)) (Neg (Nat 4))) (Neg (Mul (Nat 3) (Nat 0)))) (pa "-3*-4 * -(3 * 0)")
testOp3 = testFactory (Mul (Nat 2) (Neg (PrefixDec "x"))) (pa "2* -(--x)")
testOp4 = testFactory (Mul (Nat 2) (Neg (PrefixDec "x"))) (pa "2*---x")
testOp5 = testFactory (Sum (Var "x") (Mul (Nat 2) (Inc "i"))) (pa "x + 2 * i++")

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