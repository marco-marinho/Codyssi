module Supplies where

import Data.List (sortBy)
import Data.Text qualified as T

data Range = Range
  { start :: Integer,
    end :: Integer
  }
  deriving (Show, Eq)

fuseRanges' :: [Range] -> [Range] -> [Range]
fuseRanges' [] acc = acc
fuseRanges' [r] acc = r : acc
fuseRanges' (r1 : r2 : rs) acc
  | end r1 >= start r2 = fuseRanges' (Range (start r1) (max (end r1) (end r2)) : rs) acc
  | otherwise = fuseRanges' (r2 : rs) (r1 : acc)

fuseRanges :: [Range] -> [Range]
fuseRanges ranges = fused
  where
    sortedRanges = sortBy (\r1 r2 -> compare (start r1) (start r2)) ranges
    fused = fuseRanges' sortedRanges []

maxUnique :: [[Range]] -> Integer -> Integer
maxUnique [] maxSoFar = maxSoFar
maxUnique [_] maxSoFar = maxSoFar
maxUnique (a : b : rs) maxSoFar = maxUnique (b : rs) newMax
  where
    fused = fuseRanges (a ++ b)
    localCount = sum $ map numberElements fused
    newMax = max maxSoFar localCount

numberElements :: Range -> Integer
numberElements r = end r - start r + 1

parseStr :: T.Text -> Range
parseStr input = Range start end
  where
    (startS, endS) = case T.splitOn (T.pack "-") input of
      [s, e] -> (s, e)
      _ -> error "Invalid range format"
    start = read (T.unpack startS) :: Integer
    end = read (T.unpack endS) :: Integer

parseLine :: String -> [Range]
parseLine line = [startS, endS]
  where
    (startS, endS) = case T.splitOn (T.pack " ") (T.pack line) of
      [s, e] -> (parseStr s, parseStr e)
      _ -> error "Invalid line format"

part1 :: String -> String
part1 input = show elements
  where
    pairs = map parseLine (lines input)
    elements = sum $ map (sum . map numberElements) pairs

part2 :: String -> String
part2 input = show elements
  where
    pairs = map parseLine (lines input)
    fusedPairs = map fuseRanges pairs
    elements = sum $ map (sum . map numberElements) fusedPairs

part3 :: String -> String
part3 input = show res
  where
    pairs = map parseLine (lines input)
    fusedPairs = map fuseRanges pairs
    res = maxUnique fusedPairs 0

solve :: Int -> String -> String
solve n input = case n of
  1 -> part1 input
  2 -> part2 input
  3 -> part3 input
  _ -> error "Invalid part number"