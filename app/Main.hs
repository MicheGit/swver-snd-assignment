module Main where
import Data.Reflection
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import System.Environment (getArgs)
import Control.Exception (throw)
import While.Parser (parseWhileProgram)
import Data.Proxy (Proxy(Proxy))

import AbstractDomains.InfiniteIntegers
import AbstractDomains.Interval
import AbstractDomains.BoundedInterval

import AbstractInterpreter
import BoundedIntervalAnalysis

bindAnalysis :: (InfInt, InfInt) -> While -> AState Interval
bindAnalysis b program = reify b computation
  where
  computation :: forall s. (Boundable s (InfInt, InfInt) Interval) => Proxy s -> AState Interval
  computation reifiedBounds =
    let result :: AState (BoundedInterval s (InfInt, InfInt))
        result = analyze program
      in AbstractInterpreter.map unbox result

main :: IO ()
main = do
  program <- getParsedProgram
  print "Insert a range to instance the Bounded Interval domain (leave blank for infinity):"
  arg1 <- getLine
  let m :: InfInt
      m = maybe (-Infinity) fromInteger (readMaybe arg1)
  print $ "Lower bound set to " ++ show m
  arg2 <- getLine
  let n :: InfInt
      n = maybe Infinity fromInteger (readMaybe arg2)
  print $ if m <= n
    then "Analyzing program with Intervals within [" ++ show m ++ ", " ++ show n ++ "]"
    else "Analyzing program with constant propagation"
  print $ bindAnalysis (m, n) program

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
