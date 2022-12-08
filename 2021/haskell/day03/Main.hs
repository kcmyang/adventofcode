module Main where

import Data.Bool (bool)
import Data.Foldable (maximumBy, minimumBy)
import Data.List (group, sort, transpose)
import Data.Ord (comparing)
import System.Environment (getArgs)

parseLine :: a -> a
parseLine = id

binToDec :: String -> Int
binToDec = foldl (\acc digit -> (2 * acc) + if digit == '0' then 0 else 1) 0

mostCommonDigit :: String -> Char
-- maximumBy automatically takes the latest occurrence; in this case the bucket of 1s
mostCommonDigit = head . maximumBy (comparing length) . group . sort

leastCommonDigit :: String -> Char
-- minimumBy automatically takes the earliest occurrence; in this case the bucket of 0s
leastCommonDigit = head . minimumBy (comparing length) . group . sort

part1 :: [String] -> Int
part1 nums = gamma * epsilon
  where
    mostCommonDigits = map mostCommonDigit $ transpose nums
    leastCommonDigits = map leastCommonDigit $ transpose nums
    gamma = binToDec mostCommonDigits
    epsilon = binToDec leastCommonDigits

computeOxygen :: [String] -> String
computeOxygen [""] = ""
computeOxygen nums = mostCommon : computeOxygen (map tail (filter ((== mostCommon) . head) nums))
  where
    mostCommon = mostCommonDigit $ head $ transpose nums

computeCO2 :: [String] -> String
computeCO2 [""] = ""
computeCO2 nums = leastCommon : computeCO2 (map tail (filter ((== leastCommon) . head) nums))
  where
    leastCommon = leastCommonDigit $ head $ transpose nums

part2 :: [String] -> Int
part2 nums = oxygen * co2
  where
    oxygen = binToDec $ computeOxygen nums
    co2 = binToDec $ computeCO2 nums

main :: IO ()
main = do
  (filename : _) <- getArgs
  contents <- readFile filename
  let input = map parseLine $ lines contents

  print $ part1 input
  print $ part2 input
