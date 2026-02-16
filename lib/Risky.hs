module Risky (solve) where

import Data.Char (isAlpha, isDigit)

isReduceable :: Char -> Char -> Bool
isReduceable a b = (isDigit a && (isAlpha b || b == '-')) || isDigit b && (isAlpha a || a == '-')

isReduceable2 :: Char -> Char -> Bool
isReduceable2 a b = (isDigit a && isAlpha b) || (isDigit b && isAlpha a)

reduce' :: (Char -> Char -> Bool) -> String -> String -> String
reduce' _ [] acc = reverse acc
reduce' _ [x] acc = reverse (x : acc)
reduce' reducer (x : y : tl) acc =
  if reducer x y
    then reduce' reducer (reverse acc ++ tl) []
    else reduce' reducer (y : tl) (x : acc)

reduce :: (Char -> Char -> Bool) -> String -> String
reduce reducer input = reduce' reducer input []

part1 :: String -> String
part1 input = show . sum $ map (length . filter isAlpha) (lines input)

part2 :: String -> String
part2 input = show . sum $ map (length . reduce isReduceable) (lines input)

part3 :: String -> String
part3 input = show . sum $ map (length . reduce isReduceable2) (lines input)

solve :: Int -> String -> String
solve n input = case n of
  1 -> part1 input
  2 -> part2 input
  3 -> part3 input
  _ -> error "Invalid part number"