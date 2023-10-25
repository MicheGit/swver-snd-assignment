module Main where
import Data.Reflection
import AI
import BoundedInterval
import Interval
import Data.Maybe (fromMaybe)
import GHC.Real (infinity)
import Text.Read (readMaybe)
import System.Environment (getArgs)
import Control.Exception (throw)
import While.Parser (parseWhileProgram)
import Data.Proxy (Proxy(Proxy))


instance (r ~ (Rational, Rational), Reifies s r) => AI (BoundedInterval s r) where
  abstractA e s = (bind Interval.Bot, s)
  abstractB = error "Not implemented"

boundedAnalysis :: (Rational, Rational) -> While -> AState Interval
boundedAnalysis b program = reify b compute
  where 
    compute :: (AI (BoundedInterval s (Rational, Rational))) => Proxy s -> AState Interval
    compute p = AI.map unbox $ analyze program


main :: IO ()
main = do
  program <- getParsedProgram
  print "Insert a range to instance the Bounded Interval domain (leave blank for infinity):"
  arg1 <- getLine
  let m :: Rational
      m = maybe infinity toRational (readMaybe arg1)
  arg2 <- getLine
  let n :: Rational
      n = maybe infinity toRational (readMaybe arg2)
  print $ boundedAnalysis (m, n) program

getParsedProgram :: IO While
getParsedProgram = do
  args <- getArgs
  filename <- case args of
        [filename] -> return filename
        _ -> do
          print "Missing filename from command line arguments."
          print "Usage: cabal run ai -- path/to/file.whl"
          throw $ userError "Expected Argument Error"
  filecontent <- readFile filename
  let result = parseWhileProgram filename filecontent
  case result of
    Right program -> return program
    Left error -> do
      print error
      throw $ userError "Syntax Error"
