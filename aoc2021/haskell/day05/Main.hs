{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Data.Array (Ix (range))
import qualified Data.HashSet as HashSet
import Data.List.Split (splitOn)
import Debug.Trace (traceShowId)
import System.Environment (getArgs)

type Point = (Int, Int)

type VentLine = (Point, Point)

-- Parse a line of input as a vent line
parseLine :: String -> VentLine
parseLine inputLine =
  let pairs = map (splitOn ",") (splitOn " -> " inputLine)
   in case pairs of
        [[x1, y1], [x2, y2]] -> ((read x1, read y1), (read x2, read y2))
        _ -> error "not a valid vent line!"

minMax :: Int -> Int -> (Int, Int)
minMax a b = if a <= b then (a, b) else (b, a)

intersectionVV :: VentLine -> VentLine -> [Point]
intersectionVV vl1 vl2
  | y11 > y12 = intersectionVV ((x1, y12), (x1, y11)) vl2
  | y21 > y22 = intersectionVV vl1 ((x2, y22), (x2, y21))
  | x1 == x2 =
    let ys = range (max y11 y21, min y12 y22)
     in map (x1,) ys
  | otherwise = []
  where
    ((x1, y11), (_, y12)) = vl1
    ((x2, y21), (_, y22)) = vl2

intersectionVH :: VentLine -> VentLine -> [Point]
intersectionVH vl1 vl2
  | y11 > y12 = intersectionVH ((x1, y12), (x1, y11)) vl2
  | x21 > x22 = intersectionVH vl1 ((x22, y2), (x21, y2))
  | x21 <= x1 && x1 <= x22 && y11 <= y2 && y2 <= y12 = [(x1, y2)]
  | otherwise = []
  where
    ((x1, y11), (_, y12)) = vl1
    ((x21, y2), (x22, _)) = vl2

intersectionVD :: VentLine -> VentLine -> [Point]
intersectionVD vl1 vl2
  | y11 > y12 = intersectionVD ((x1, y12), (x1, y11)) vl2
  -- ensure second line goes left to right
  | x21 > x22 = intersectionVD vl1 ((x22, y22), (x21, y21))
  -- slope = -1
  -- y - y' = -(x - x') => y = -x + x' + y', evaluate at x = x1, x' = x21, y' = y21
  | y21 >= y22 =
    let y = (- x1) + x21 + y21
     in [(x1, y) | max y11 y22 <= y && y <= min y12 y21]
  -- slope = 1
  -- y - y' = x - x' => y = x - x' + y', evaluate at x = x1, x' = x21, y' = y21
  | otherwise =
    let y = x1 - x21 + y21
     in [(x1, y) | max y11 y21 <= y && y <= min y12 y22]
  where
    ((x1, y11), (_, y12)) = vl1
    ((x21, y21), (x22, y22)) = vl2

intersectionHV :: VentLine -> VentLine -> [Point]
intersectionHV vl1 vl2 = intersectionVH vl2 vl1

intersectionHH :: VentLine -> VentLine -> [Point]
intersectionHH vl1 vl2
  | x11 > x12 = intersectionHH ((x12, y1), (x11, y1)) vl2
  | x21 > x22 = intersectionHH vl1 ((x22, y2), (x21, y2))
  | y1 == y2 =
    let xs = range (max x11 x21, min x12 x22)
     in map (,y1) xs
  | otherwise = []
  where
    ((x11, y1), (x12, _)) = vl1
    ((x21, y2), (x22, _)) = vl2

intersectionHD :: VentLine -> VentLine -> [Point]
intersectionHD vl1 vl2
  | x11 > x12 = intersectionHD ((x12, y1), (x11, y1)) vl2
  -- ensure second line goes bottom to top
  | y21 > y22 = intersectionHD vl1 ((x22, y22), (x21, y21))
  -- slope = -1
  -- y - y' = -(x - x') => x = x' - y + y', evaluate at y = y1, x' = x21, y' = y21
  | x21 >= x22 =
    let x = x21 - y1 + y21
     in [(x, y1) | max x11 x22 <= x && x <= min x12 x21]
  -- slope = 1
  -- y - y' = x - x' => x = x' + y - y', evaluate at y = y1, x' = x21, y' = y21
  | otherwise =
    let x = x21 + y1 - y21
     in [(x, y1) | max x11 x21 <= x && x <= min x12 x22]
  where
    ((x11, y1), (x12, _)) = vl1
    ((x21, y21), (x22, y22)) = vl2

intersectionDV :: VentLine -> VentLine -> [Point]
intersectionDV vl1 vl2 = intersectionVD vl2 vl1

intersectionDH :: VentLine -> VentLine -> [Point]
intersectionDH vl1 vl2 = intersectionHD vl2 vl1

intersectionDD :: VentLine -> VentLine -> [Point]
intersectionDD vl1 vl2
  -- ensure lines go left to right
  | x11 > x12 = intersectionDD ((x12, y12), (x11, y11)) vl2
  | x21 > x22 = intersectionDD vl1 ((x22, y22), (x21, y21))
  -- both have slope 1
  | y11 <= y12 && y21 <= y22 =
    if x21 - x11 == y21 - y11
      then map (\x -> (x, y11 + x - x11)) (range (max x11 x21, min x12 x22))
      else []
  -- both have slope -1
  | y11 > y12 && y21 > y22 =
    if x21 - x11 == y11 - y21
      then map (\x -> (x, y11 - x + x11)) (range (max x11 x21, min x12 x22))
      else []
  -- if different slopes, ensure first has slope 1 and second has slope -1
  | y11 > y12 = intersectionDD vl2 vl1
  -- vl1: y - y' = x - x' => y = y' + x - x', evaluate at x' = x11, y' = y11
  -- vl2: y - y' = -(x - x') => y = y' - x + x', evaluate at x' = x21, y' = y21
  -- => y11 + x - x11 = y21 - x + x21 => x = (y21 + x21 - y11 + x11)/2
  | otherwise =
    let xx = y21 + x21 - y11 + x11
     in if odd xx
          then []
          else
            let x = xx `div` 2
                y = y11 + x - x11
             in [(x, y)]
  where
    ((x11, y11), (x12, y12)) = vl1
    ((x21, y21), (x22, y22)) = vl2

-- Get the intersection points of two vent lines... seems to be broken :(
intersection2 :: VentLine -> VentLine -> [Point]
intersection2 vl1 vl2
  | x11 == x12 =
    if
        | x21 == x22 -> intersectionVV vl1 vl2
        | y21 == y22 -> intersectionVH vl1 vl2
        | otherwise -> intersectionVD vl1 vl2
  | y11 == y12 =
    if
        | x21 == x22 -> intersectionHV vl1 vl2
        | y21 == y22 -> intersectionHH vl1 vl2
        | otherwise -> intersectionHD vl1 vl2
  | otherwise =
    if
        | x21 == x22 -> intersectionDV vl1 vl2
        | y21 == y22 -> intersectionDH vl1 vl2
        | otherwise -> intersectionDD vl1 vl2
  where
    ((x11, y11), (x12, y12)) = vl1
    ((x21, y21), (x22, y22)) = vl2

-- Get the set of points contained in this line
points :: VentLine -> HashSet.HashSet Point
points ((x1, y1), (x2, y2))
  | x1 == x2 = HashSet.fromList $ map (x1,) (range (min y1 y2, max y1 y2))
  | y1 == y2 = HashSet.fromList $ map (,y1) (range (min x1 x2, max x1 x2))
  | otherwise =
    let ((lx, ly), (rx, ry)) = if x1 <= x2 then ((x1, y1), (x2, y2)) else ((x2, y2), (x1, y1))
        mapFun = if ly <= ry then \x -> (x, ly + x - lx) else \x -> (x, ly - x + lx)
     in HashSet.fromList $ map mapFun (range (lx, rx))

-- Get the intersection points of two vent lines (MUCH slower, but works)
intersection :: VentLine -> VentLine -> HashSet.HashSet Point
intersection vl1 vl2 = HashSet.intersection (points vl1) (points vl2)

-- Count the number of distinct points where two or more lines intersect
countCollisions ::
  HashSet.HashSet Point -> -- collisions
  [VentLine] -> -- seen vent lines
  [VentLine] -> -- unseen vent lines
  Int -- collision count
countCollisions collisions _ [] = HashSet.size collisions
countCollisions collisions seen (ventLine : unseen) =
  let collisions' = HashSet.unions $ collisions : map (intersection ventLine) seen
   in countCollisions collisions' (ventLine : seen) unseen

-- Part 1: find number of points where at least two horizontal or vertical lines cross
part1 :: [VentLine] -> Int
part1 ventLines =
  countCollisions
    HashSet.empty
    []
    (filter (\((x1, y1), (x2, y2)) -> x1 == x2 || y1 == y2) ventLines)

-- Part 2: find number of points where any two lines cross
part2 :: [VentLine] -> Int
part2 = countCollisions HashSet.empty []

main :: IO ()
main = do
  (filename : _) <- getArgs
  contents <- readFile filename
  let inputLines = lines contents
      ventLines = map parseLine inputLines

  print $ part1 ventLines
  print $ part2 ventLines
