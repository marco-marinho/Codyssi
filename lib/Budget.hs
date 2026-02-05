module Budget (solve) where

import Data.List (sortBy)
import Data.Ord (Down (..), comparing)

lsum :: [Integer] -> Integer -> Integer
lsum [] !acc = acc
lsum (x : xs) !acc = lsub xs (acc + x)

lsub :: [Integer] -> Integer -> Integer
lsub [] !acc = acc
lsub (x : xs) acc = lsum xs (acc - x)

part1 :: String -> String
part1 input = res
  where
    vals = map read (lines input) :: [Integer]
    res = show $ sum vals

part2 :: String -> String
part2 input = res
  where
    vals = map read (lines input) :: [Integer]
    sortedVals = sortBy (comparing Down) vals
    res = show $ sum (drop 20 sortedVals)

part3 :: String -> String
part3 input = res
  where
    vals = map read (lines input) :: [Integer]
    res = show $ lsum vals 0

solve :: Int -> String -> String
solve n input = case n of
  1 -> part1 input
  2 -> part2 input
  3 -> part3 input
  _ -> error "Invalid part number"
