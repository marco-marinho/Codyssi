module Aeolian (solve) where

import Data.Char (digitToInt, isDigit)

toMemUnit :: Char -> Integer
toMemUnit c =
  if isDigit c
    then fromIntegral (digitToInt c)
    else fromIntegral (fromEnum c - fromEnum 'A' + 1)

compressLine :: String -> String
compressLine line = compressed
  where
    len = length line
    prefixLen = len `div` 10
    removed = len - 2 * prefixLen
    compressed = take prefixLen line ++ show removed ++ drop (prefixLen + removed) line

runLengthEncode' :: String -> (Char, Int) -> String -> String
runLengthEncode' [] (c, count) acc = reverse ([c] ++ reverse (show count) ++ acc)
runLengthEncode' (x : xs) (c, count) acc =
  if x == c
    then runLengthEncode' xs (c, count + 1) acc
    else runLengthEncode' xs (x, 1) ([c] ++ reverse (show count) ++ acc)

runLengthEncode :: String -> String
runLengthEncode input = case input of
  [] -> ""
  (c : tl) -> runLengthEncode' tl (c, 1) ""

part1 :: String -> String
part1 input = show memCost
  where
    memCost = sum $ map (sum . map toMemUnit) (lines input)

part2 :: String -> String
part2 input = show memCost
  where
    memCost = sum $ map ((sum . map toMemUnit) . compressLine) (lines input)

part3 :: String -> String
part3 input = show memCost
  where
    compressedLines = map runLengthEncode (lines input)
    memCost = sum $ map (sum . map toMemUnit) compressedLines

solve :: Int -> String -> String
solve part input = case part of
  1 -> part1 input
  2 -> part2 input
  3 -> part3 input
  _ -> error "Invalid part number"