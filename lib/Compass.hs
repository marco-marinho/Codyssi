module Compass (solve) where

calibrate :: [Int] -> [Char] -> Int -> Int
calibrate [] _ acc = acc
calibrate _ [] acc = acc
calibrate (x : xs) (d : ds) acc = case d of
  '+' -> calibrate xs ds (acc + x)
  '-' -> calibrate xs ds (acc - x)
  _ -> error "Invalid operation"

makeTwoDigits :: [Int] -> [Int] -> [Int]
makeTwoDigits [] acc = reverse acc
makeTwoDigits [_] _ = error "Odd number of digits"
makeTwoDigits (x : y : xs) acc = makeTwoDigits xs (10 * x + y : acc)

part1 :: String -> String
part1 input = show res
  where
    inputLines = lines input
    numbers = map (read :: String -> Int) (take (length inputLines - 1) inputLines)
    operations = last inputLines
    res = case numbers of
      (x : xs) -> calibrate xs operations x
      [] -> error "No numbers provided"

part2 :: String -> String
part2 input = show res
  where
    inputLines = lines input
    numbers = map (read :: String -> Int) (take (length inputLines - 1) inputLines)
    operations = last inputLines
    res = case numbers of
      (x : xs) -> calibrate xs (reverse operations) x
      [] -> error "No numbers provided"

part3 :: String -> String
part3 input = show res
  where
    inputLines = lines input
    numbers = map (read :: String -> Int) (take (length inputLines - 1) inputLines)
    operations = last inputLines
    res = case makeTwoDigits numbers [] of
      (x : xs) -> calibrate xs (reverse operations) x
      [] -> error "No numbers provided"

solve :: Int -> String -> String
solve n input = case n of
  1 -> part1 input
  2 -> part2 input
  3 -> part3 input
  _ -> error "Invalid part number"