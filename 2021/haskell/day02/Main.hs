module Main where

import System.Environment (getArgs)

data Direction = Forward | Down | Up

data Command = Command
  { direction :: Direction,
    magnitude :: Int
  }

parseLine :: [Char] -> Command
parseLine line
  | direction == "forward" = Command Forward (read magnitude)
  | direction == "down" = Command Down (read magnitude)
  | direction == "up" = Command Up (read magnitude)
  | otherwise = error ("Invalid direction " ++ direction)
  where
    [direction, magnitude] = words line

part1movement :: (Int, Int) -> Command -> (Int, Int)
part1movement (hpos, depth) (Command direction magnitude) = case direction of
  Forward -> (hpos + magnitude, depth)
  Down -> (hpos, depth + magnitude)
  Up -> (hpos, depth - magnitude)

part1 :: Foldable t => t Command -> Int
part1 commands =
  hpos * depth
  where
    (hpos, depth) = foldl part1movement (0, 0) commands

part2movement :: (Int, Int, Int) -> Command -> (Int, Int, Int)
part2movement (hpos, depth, aim) (Command direction magnitude) = case direction of
  Forward -> (hpos + magnitude, depth + aim * magnitude, aim)
  Down -> (hpos, depth, aim + magnitude)
  Up -> (hpos, depth, aim - magnitude)

part2 :: Foldable t => t Command -> Int
part2 commands =
  hpos * depth
  where
    (hpos, depth, _) = foldl part2movement (0, 0, 0) commands

main :: IO ()
main = do
  (filename : _) <- getArgs
  contents <- readFile filename
  let input = map parseLine $ lines contents

  print $ part1 input
  print $ part2 input
