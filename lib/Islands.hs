module Islands (solve) where

import Data.List (sort)
import Text.Read (readMaybe)

manhatanDistance :: (Integer, Integer) -> (Integer, Integer) -> Integer
manhatanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

parseLine :: String -> (Integer, Integer)
parseLine line = case readMaybe line :: Maybe (Integer, Integer) of
  Just pair -> pair
  Nothing -> error "Invalid line format"

minDistance :: (Integer, Integer) -> [(Integer, Integer)] -> Integer -> Integer
minDistance _ [] acc = acc
minDistance point others acc = minDistance (a1, b1) nextOthers (acc + dist)
  where
    distances = map (\(a, b) -> (manhatanDistance point (a, b), a, b)) others
    sortedDistances = sort distances
    ((dist, a1, b1), rest) = case sortedDistances of
      [] -> error "Empty points"
      (d : ds) -> (d, ds)
    nextOthers = map (\(_, a, b) -> (a, b)) rest

part1 :: String -> String
part1 input = show res
  where
    points = map parseLine (lines input)
    distances = map (manhatanDistance (0, 0)) points
    minDist = minimum distances
    maxDist = maximum distances
    res = maxDist - minDist

part2 :: String -> String
part2 input = show res
  where
    points = map parseLine (lines input)
    distances = map (\(a, b) -> (manhatanDistance (0, 0) (a, b), a, b)) points
    ((_, a1, b1), rest) = case sort distances of
      [] -> error "No points provided"
      (d : ds) -> (d, ds)
    secondDistances = map (\(_, a, b) -> (manhatanDistance (a1, b1) (a, b), a, b)) rest
    ((res, _, _), _, _) = case sort secondDistances of
      [] -> error "No other points provided"
      (d : _) -> (d, a1, b1)

part3 :: String -> String
part3 input = show res
  where
    points = map parseLine (lines input)
    res = minDistance (0, 0) points 0

solve :: Int -> String -> String
solve part input = case part of
  1 -> part1 input
  2 -> part2 input
  3 -> part3 input
  _ -> error "Invalid part number"