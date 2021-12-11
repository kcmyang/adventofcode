module Main where

import System.Environment (getArgs)

part1 :: (Num a1, Ord a2) => [a2] -> a1
part1 nums = sum $ zipWith (\a b -> if a < b then 1 else 0) nums (tail nums)

part2 :: (Num a1, Ord a2) => [a2] -> a1
part2 nums = sum $ zipWith (\a b -> if a < b then 1 else 0) nums (tail $ tail $ tail nums)

main :: IO ()
main = do
  (filename : _) <- getArgs
  contents <- readFile filename
  let input = map read $ lines contents :: [Int]

  print $ part1 input
  print $ part2 input
