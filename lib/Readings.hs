module Readings (solve) where

import Numeric (readBin, readHex, readOct)

base65 :: Int -> Char
base65 v
  | v < 10 = toEnum (v + fromEnum '0')
  | v < 36 = toEnum (v - 10 + fromEnum 'A')
  | v < 62 = toEnum (v - 36 + fromEnum 'a')
  | v == 62 = '!'
  | v == 63 = '@'
  | v == 64 = '#'
  | otherwise = error "Invalid value for base 65"

toBase65 :: Int -> String -> String
toBase65 0 acc = if null acc then "0" else acc
toBase65 n acc = toBase65 q acc'
  where
    (q, r) = n `divMod` 65
    acc' = base65 r : acc

parser :: String -> Int -> Int
parser v b = case b of
  10 -> read v :: Int
  8 -> case readOct v of
    [(n, "")] -> n
    _ -> error "Invalid octal number"
  16 -> case readHex v of
    [(n, "")] -> n
    _ -> error "Invalid hexadecimal number"
  2 -> case readBin v of
    [(n, "")] -> n
    _ -> error "Invalid binary number"
  _ -> error "Unsupported base"

part1 :: String -> String
part1 input = res
  where
    bases =
      map
        ( \s -> case words s of
            (_ : b : _) -> (read b :: Int)
            _ -> error "Invalid input format"
        )
        (lines input)
    res = show $ sum bases

part2 :: String -> String
part2 input = res
  where
    bases =
      map
        ( \s -> case words s of
            (a : b : _) -> parser a (read b :: Int)
            _ -> error "Invalid input format"
        )
        (lines input)
    res = show $ sum bases

part3 :: String -> String
part3 input = res
  where
    res = toBase65 (read $ part2 input) []

solve :: Int -> String -> String
solve n input = case n of
  1 -> part1 input
  2 -> part2 input
  3 -> part3 input
  _ -> error "Invalid part number"