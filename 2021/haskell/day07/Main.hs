module Main where

import Data.List.Split (splitOn)
import System.Environment (getArgs)

-- Related idea: geometric median
part1 :: [Int] -> Int
part1 positions =
  minimum $ map fuelCost [xmin .. xmax]
  where
    xmin = minimum positions
    xmax = maximum positions
    fuelCost x = sum $ map (\n -> abs (n - x)) positions

part2 :: [Int] -> Int
part2 positions =
  minimum $ map fuelCost [xmin .. xmax]
  where
    xmin = minimum positions
    xmax = maximum positions
    triangle d = (d * (d + 1)) `div` 2
    fuelCost x = sum $ map (\n -> triangle $ abs (n - x)) positions

main :: IO ()
main = do
  (filename : _) <- getArgs
  contents <- readFile filename
  let input = head $ lines contents
      positions = map read $ splitOn "," input :: [Int]

  print $ part1 positions
  print $ part2 positions
