module Cyclops (solve) where

import Data.Array

parseInput :: [[Int]] -> Array (Int, Int) Int
parseInput [] = array ((0, 0), (0, 0)) []
parseInput rows@(firstRow : _) = oarray
  where
    numRows = length rows
    numCols = length firstRow
    oarray = array ((0, 0), (numRows - 1, numCols - 1)) [((r, c), rows !! r !! c) | r <- [0 .. numRows - 1], c <- [0 .. numCols - 1]]

dangerLevelRow :: Array (Int, Int) Int -> Int -> Int
dangerLevelRow arr r = sum [arr ! (r, c) | c <- [0 .. colBounds]]
  where
    (_, (_, colBounds)) = bounds arr

dangerLevelCol :: Array (Int, Int) Int -> Int -> Int
dangerLevelCol arr c = sum [arr ! (r, c) | r <- [0 .. rowBounds]]
  where
    (_, (rowBounds, _)) = bounds arr

generateCosts :: Array (Int, Int) Int -> Array (Int, Int) Int
generateCosts arr = costs
  where
    bnds@(_, (rowBounds, colBounds)) = bounds arr
    costs = array bnds [((x, y), calc x y) | x <- [0 .. rowBounds], y <- [0 .. colBounds]]
    calc x y
      | x == 0 && y == 0 = arr ! (x, y)
      | x == 0 = costs ! (x, y - 1) + arr ! (x, y)
      | y == 0 = costs ! (x - 1, y) + arr ! (x, y)
      | otherwise = min (costs ! (x - 1, y)) (costs ! (x, y - 1)) + arr ! (x, y)

part1 :: String -> String
part1 input = show res
  where
    grid = parseInput (map (map read . words) (lines input))
    (_, (boundsRows, boundsCols)) = bounds grid
    dangerRows = [dangerLevelRow grid r | r <- [0 .. boundsRows]]
    dangerCols = [dangerLevelCol grid c | c <- [0 .. boundsCols]]
    res = minimum (dangerRows ++ dangerCols)

part2 :: String -> String
part2 input = show res
  where
    grid = parseInput (map (map read . words) (lines input))
    costs = generateCosts grid
    res = costs ! (14, 14)

part3 :: String -> String
part3 input = show res
  where
    grid = parseInput (map (map read . words) (lines input))
    costs = generateCosts grid
    (_, (boundsRows, boundsCols)) = bounds grid
    res = costs ! (boundsRows, boundsCols)

solve :: Int -> String -> String
solve n input = case n of
  1 -> part1 input
  2 -> part2 input
  3 -> part3 input
  _ -> error "Invalid part number"