module Arithmetic where

import Data.List (maximumBy, sort)
import Debug.Trace (traceShowId)

applyPricing :: Integer -> Integer -> Integer -> Integer -> Integer
applyPricing ep mp adt x = ((x ^ ep) * mp) + adt

parse :: String -> (Integer, Integer, Integer, [Integer])
parse input = (adt, mp, ep, roomQualities)
  where
    inputLines = lines input
    functionsArgLines = take 3 inputLines
    functionArgs = map (\s -> read (last (words s)) :: Integer) functionsArgLines
    (adt, mp, ep) = case functionArgs of
      [a, b, c] -> (a, b, c)
      _ -> error "Invalid function arguments"
    roomQualities = map (\s -> read s :: Integer) (drop 4 inputLines)

part1 :: String -> String
part1 input = res
  where
    (adt, mp, ep, roomQualities) = parse input
    prices = map (applyPricing ep mp adt) roomQualities
    sortedPrices = sort prices
    res = show (sortedPrices !! 50)

part2 :: String -> String
part2 input = res
  where
    (adt, mp, ep, roomQualities) = parse input
    evenQualities = sum $ filter even roomQualities
    res = show (applyPricing ep mp adt evenQualities)

part3 :: String -> String
part3 input = res
  where
    (adt, mp, ep, roomQualities) = parse input
    prices = map (\s -> (s, applyPricing ep mp adt s)) roomQualities
    filteredPrices = filter (\(_, s) -> s <= 15000000000000) prices
    res = show $ maximumBy (\(a, _) (b, _) -> compare a b) filteredPrices

solve :: Int -> String -> String
solve n input = case n of
  1 -> part1 input
  2 -> part2 input
  3 -> part3 input
  _ -> error "Invalid part number"