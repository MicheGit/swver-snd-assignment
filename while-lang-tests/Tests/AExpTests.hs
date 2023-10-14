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

testOp1 = testFactory (Mul (Nat 2) (Neg (Nat 3))) (pa "2* -3")
testOp2 = testFactory (Mul (Mul (Neg (Nat 3)) (Neg (Nat 4))) (Neg (Mul (Nat 3) (Nat 0)))) (pa "-3*-4 * -(3 * 0)")

aexprTests = TestList
    [ testVariableName
    , testNat
    , testVar
    , testNeg1
    , testNeg2
    , testOp1
    , testOp2
    ]