module Main where

import Debug.Trace (traceShowId)
import System.Environment (getArgs)

part1 :: [String] -> Int
part1 _ = 0

part2 :: [String] -> Int
part2 _ = 0

main :: IO ()
main = do
  (filename : _) <- getArgs
  contents <- readFile filename
  let input = lines contents

  print $ part1 input
  print $ part2 input
