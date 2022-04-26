module Main where

import Data.Array (Array, Ix (range), array, elems, (!), (//))
import Data.List.Split (splitOn)
import Debug.Trace (traceShowId)
import System.Environment (getArgs)

type Counts = Array Int Int

-- Parse a line of input
parseLine :: String -> Counts
parseLine inputLine =
  let nums = map read (splitOn "," inputLine) :: [Int]
   in foldl (\arr n -> arr // [(n, arr ! n + 1)]) (array (0, 8) [(i, 0) | i <- [0 .. 8]]) nums

-- Simulate d days of fish and get the total fish at the end
simulate :: Int -> Counts -> Int
simulate 0 counts = sum (elems counts)
simulate d counts =
  let counts' = array (0, 8) ((8, counts ! 0) : [(i - 1, counts ! i) | i <- [1 .. 8]])
      counts'' = counts' // [(6, counts' ! 6 + counts ! 0)]
   in simulate (d - 1) counts''

-- Part 1: simulate 80 days of fish
part1 :: Counts -> Int
part1 = simulate 80

-- Part 2: simulate 256 days of fish
part2 :: Counts -> Int
part2 = simulate 256

main :: IO ()
main = do
  (filename : _) <- getArgs
  contents <- readFile filename
  let input = head $ lines contents
      counts = parseLine input

  print $ part1 counts
  print $ part2 counts
