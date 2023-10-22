module Tests.StmtTests where
import Tests.Common (assumeParseString, testFactory, parse)
import Test.HUnit (Test (TestList))
import While.Language (Stmt(Assg, Brnc, Cons, Skip, Loop))

testAssign :: Test
testAssign = testFactory (Assg "x" (parse "45")) (parse "x :=45")

testBranch :: Test
testBranch = testFactory (Cons (Brnc (parse "3 < x++") (parse "x := 4") Skip) (parse "x := 5")) (parse "if 3 < x++ then x:=4 else skip; x := 5")

testWhile :: Test
testWhile = testFactory (Cons (parse "x := 1") 
        (Cons (Loop (parse "--y > 0") (Cons (parse "x := x * y") Skip))
                (parse "z := x"))) (parse "x:=1; while --y > 0 do { x:=x * y ; skip } ; z := x")

stmtTests = TestList
    [ testAssign
    , testBranch
    , testWhile
    ]