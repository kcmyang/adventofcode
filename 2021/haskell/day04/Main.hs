module Main where

import Data.List (find, partition, transpose)
import Data.List.Split (splitOn)
import Debug.Trace (traceShowId)
import System.Environment (getArgs)

-- A cell consists of a number and its marked state
type Cell = (Int, Bool)

type Row = [Cell]

type Board = [Row]

parseBoard :: [String] -> Board
parseBoard = map (\line -> zip (map read (words line)) (repeat False))

traceBoard :: Board -> Board
traceBoard = map traceShowId

-- Apply the called number to the row
applyCalledNumberToRow :: Int -> Row -> Row
applyCalledNumberToRow _ [] = []
applyCalledNumberToRow num ((content, marked) : rest)
  | content == num = (content, True) : rest
  | otherwise = (content, marked) : applyCalledNumberToRow num rest

-- Apply the called number to the board
applyCalledNumberToBoard :: Int -> Board -> Board
applyCalledNumberToBoard num = map (applyCalledNumberToRow num)

-- Determine whether a row has a bingo
rowHasBingo :: Row -> Bool
rowHasBingo = all snd

-- Determine whether a board has a bingo
boardHasBingo :: Board -> Bool
boardHasBingo board = any rowHasBingo board || any rowHasBingo (transpose board)

-- Get the partial score of the row: this is just the sum of the unmarked cells
scoreRow :: Row -> Int
scoreRow = sum . map fst . filter (not . snd)

-- Get the score of the board: (sum of the unmarked cells) * (last called number)
scoreBoard :: Int -> Board -> Int
scoreBoard num board = num * (sum . map scoreRow) board

-- Find the first winning board's score
part1 :: [Int] -> [Board] -> Int
part1 _ [] = 0
part1 [] _ = 0
part1 (num : nums) boards =
  let appliedBoards = map (applyCalledNumberToBoard num) boards
   in case find boardHasBingo appliedBoards of
        Nothing -> part1 nums appliedBoards
        Just board -> scoreBoard num (traceBoard board)

-- Find the last winning board's score
part2Helper :: Int -> [Int] -> [Board] -> Int
part2Helper score [] _ = score
part2Helper score (num : nums) boards =
  let appliedBoards = map (applyCalledNumberToBoard num) boards
      (winningBoards, otherBoards) = partition boardHasBingo appliedBoards
      newScore = case winningBoards of
        [] -> score
        (w : _) -> scoreBoard num (traceBoard w)
   in part2Helper newScore nums otherBoards

-- Find the last winning board's score
part2 :: [Int] -> [Board] -> Int
part2 = part2Helper 0

main :: IO ()
main = do
  (filename : _) <- getArgs
  contents <- readFile filename
  let (numsToParse : _ : boardsToParse) = lines contents
      nums = map read $ splitOn "," numsToParse :: [Int]
      boards = map parseBoard $ splitOn [""] boardsToParse

  print $ part1 nums boards
  print $ part2 nums boards
