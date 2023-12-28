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

main :: IO ()
main = do
  (filename, program) <- getParsedProgram
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
  let invariants = bindAnalysisLog (m, n) program
  writeFile (filename ++ ".inv") (show invariants)

getParsedProgram :: IO (String, While)
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
    Right program -> return (filename, program)
    Left error -> do
      print error
      throw $ userError "Syntax Error"
