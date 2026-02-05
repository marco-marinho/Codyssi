module Sensors (solve) where

sumSensors :: [Bool] -> Int -> Int -> Int
sumSensors [] _ !acc = acc
sumSensors (x : xs) idx !acc = case x of
  True -> sumSensors xs (idx + 1) (acc + idx)
  False -> sumSensors xs (idx + 1) acc

andSensors :: [Bool] -> [Bool] -> [Bool]
andSensors (a : b : xs) !acc = case (a, b) of
  (True, True) -> orSensors xs (True : acc)
  _ -> orSensors xs (False : acc)
andSensors _ !acc = reverse acc

orSensors :: [Bool] -> [Bool] -> [Bool]
orSensors (a : b : xs) !acc = case (a, b) of
  (False, False) -> andSensors xs (False : acc)
  _ -> andSensors xs (True : acc)
orSensors _ !acc = reverse acc

reduceSensors :: [Bool] -> Int -> Int
reduceSensors [] _ = error "Empty sensor list"
reduceSensors [x] !acc = if x then acc + 1 else acc
reduceSensors xs !acc = reduceSensors nsensors nacc
  where
    nsensors = andSensors xs []
    nacc = acc + length (filter id xs)

part1 :: String -> String
part1 input = res
  where
    sensors = map (== "TRUE") (lines input)
    res = show $ sumSensors sensors 1 0

part2 :: String -> String
part2 input = res
  where
    sensors = map (== "TRUE") (lines input)
    res = show $ length $ filter id $ andSensors sensors []

part3 :: String -> String
part3 input = res
  where
    sensors = map (== "TRUE") (lines input)
    res = show $ reduceSensors sensors 0

solve :: Int -> String -> String
solve n input = case n of
  1 -> part1 input
  2 -> part2 input
  3 -> part3 input
  _ -> error "Invalid part number"
