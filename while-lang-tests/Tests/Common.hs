module Tests.Common where

import Test.HUnit
import While.Parser
import While.Language

assumeParseString :: Parser a -> String -> a
assumeParseString p text = case parseString p text of
    Left e          -> error (show e)
    Right parsed    -> parsed


testFactory :: (Eq a, Show a) => a -> a -> Test
testFactory expected = TestCase . assertEqual ("should be " ++ show expected) expected

parse :: (Parsable a) => String -> a
parse = assumeParseString parseExp