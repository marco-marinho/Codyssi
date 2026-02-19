module Games (solve) where

import Data.Char (isDigit, isLetter, isLowerCase, isUpperCase)

parseChar :: Char -> Integer
parseChar c | isDigit c = fromIntegral (fromEnum c - fromEnum '0')
parseChar c | isLetter c && isUpperCase c = fromIntegral (fromEnum c - fromEnum 'A' + 10)
parseChar c | isLetter c && isLowerCase c = fromIntegral (fromEnum c - fromEnum 'a' + 36)
parseChar _ = error "Invalid character for base 62"

base68 :: Integer -> Char
base68 v
  | v < 10 = toEnum (fromIntegral v + fromEnum '0')
  | v < 36 = toEnum (fromIntegral (v - 10) + fromEnum 'A')
  | v < 62 = toEnum (fromIntegral (v - 36) + fromEnum 'a')
  | v == 62 = '!'
  | v == 63 = '@'
  | v == 64 = '#'
  | v == 65 = '$'
  | v == 66 = '%'
  | v == 67 = '^'
  | otherwise = error "Invalid value for base 65"

toBase68 :: Integer -> String -> String
toBase68 0 !acc = if null acc then "0" else acc
toBase68 n !acc = toBase68 q acc'
  where
    (q, r) = n `divMod` 68
    acc' = base68 r : acc

searchBase :: Integer -> Integer
searchBase v = go 2
  where
    go b
      | v `div` (b ^ (4 :: Integer)) == 0 = b
      | otherwise = go (b + 1)

getValue :: String -> Integer -> Integer
getValue str base = res
  where
    values = zipWith (\i c -> parseChar c * (base ^ i)) [0 :: Integer ..] (reverse str)
    res = sum values

parseLine :: String -> (String, Integer)
parseLine line = case words line of
  (num : base : _) -> (num, read base :: Integer)
  _ -> error "Invalid input format"

part1 :: String -> String
part1 input = show res
  where
    valuesBases = map parseLine (lines input)
    values = map (uncurry getValue) valuesBases
    res = maximum values

part2 :: String -> String
part2 input = res
  where
    valuesBases = map parseLine (lines input)
    values = map (uncurry getValue) valuesBases
    toConvert = sum values
    res = toBase68 toConvert []

part3 :: String -> String
part3 input = show res
  where
    valuesBases = map parseLine (lines input)
    values = map (uncurry getValue) valuesBases
    toFindBase = sum values
    res = searchBase toFindBase

solve :: Int -> String -> String
solve n input = case n of
  1 -> part1 input
  2 -> part2 input
  3 -> part3 input
  _ -> error "Invalid part number"