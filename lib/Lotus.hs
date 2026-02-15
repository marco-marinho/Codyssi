module Lotus (solve) where

import Data.Char (isAlpha, isUpper)

charValue :: Char -> Integer
charValue c
  | isAlpha c && isUpper c = fromIntegral (fromEnum c - fromEnum 'A' + 27)
  | isAlpha c = fromIntegral (fromEnum c - fromEnum 'a' + 1)
  | otherwise = 0

fixValue :: Integer -> Integer
fixValue x = ((x - 1) `mod` 52) + 1

corruptedValue :: Integer -> Integer
corruptedValue x = fixValue nVal
  where
    nVal = (x * 2) - 5

sumValues' :: String -> Integer -> Integer -> Integer
sumValues' [] _ acc = acc
sumValues' (x : xs) n acc = sumValues' xs charVal (acc + charVal)
  where
    charVal =
      if isAlpha x
        then charValue x
        else corruptedValue n

sumValues :: String -> Integer
sumValues input = sumValues' rest initialVal initialVal
  where
    (c, rest) = case input of
      [] -> error "Empty input"
      (x : xs) -> (x, xs)
    initialVal = charValue c

part1 :: String -> String
part1 input = show res
  where
    filtered = filter isAlpha input
    res = length filtered

part2 :: String -> String
part2 input = show res
  where
    values = map charValue input
    res = sum values

part3 :: String -> String
part3 input = show res
  where
    res = sumValues input

solve :: Int -> String -> String
solve part input = case part of
  1 -> part1 input
  2 -> part2 input
  3 -> part3 input
  _ -> error "Invalid part number"