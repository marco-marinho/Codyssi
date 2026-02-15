module Siren (solve) where

import Control.Monad.ST
import Data.List.Split
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV

doSwap :: [Int] -> [(Int, Int)] -> Int -> Int
doSwap freqs swaps target = runST $ do
  frequencies <- V.unsafeThaw (V.fromList freqs)

  let swap swapList = do
        case swapList of
          [] -> return ()
          (a, b) : tl -> do
            aFreq <- MV.unsafeRead frequencies a
            bFreq <- MV.unsafeRead frequencies b
            MV.unsafeWrite frequencies a bFreq
            MV.unsafeWrite frequencies b aFreq
            swap tl
  swap swaps
  MV.unsafeRead frequencies target

doTrippleSwap :: [Int] -> [(Int, Int, Int)] -> Int -> Int
doTrippleSwap freqs swaps target = runST $ do
  frequencies <- V.unsafeThaw (V.fromList freqs)

  let swap swapList = do
        case swapList of
          [] -> return ()
          (a, b, c) : tl -> do
            aFreq <- MV.unsafeRead frequencies a
            bFreq <- MV.unsafeRead frequencies b
            cFreq <- MV.unsafeRead frequencies c
            MV.unsafeWrite frequencies b aFreq
            MV.unsafeWrite frequencies c bFreq
            MV.unsafeWrite frequencies a cFreq
            swap tl
  swap swaps
  MV.unsafeRead frequencies target

doSwapBlock :: [Int] -> [(Int, Int)] -> Int -> Int
doSwapBlock freqs swaps target = runST $ do
  frequencies <- V.unsafeThaw (V.fromList freqs)
  let numFrequencies = MV.length frequencies
  
  let swap swapList = do
        case swapList of
          [] -> return ()
          (a, b) : tl -> do
            let swapLen = minimum [abs(a - b), numFrequencies - a, numFrequencies - b]
            temp <- MV.unsafeNew swapLen
            let sliceA = MV.unsafeSlice a swapLen frequencies
                sliceB = MV.unsafeSlice b swapLen frequencies
            MV.unsafeCopy temp sliceA
            MV.unsafeCopy sliceA sliceB
            MV.unsafeCopy sliceB temp
            swap tl
  swap swaps
  MV.unsafeRead frequencies target

parseSwap :: String -> (Int, Int)
parseSwap line = case splitOn "-" line of
  [a, b] -> (read a - 1, read b - 1)
  _ -> error "Invalid swap format"

parseTrippleSwap :: (String, String) -> (Int, Int, Int)
parseTrippleSwap (line1, line2) = case (splitOn "-" line1, splitOn "-" line2) of
  ([a, b], [c, _]) -> (read a - 1, read b - 1, read c - 1)
  _ -> error "Invalid tripple swap format"

parseTrippleSwapBlock :: [String] -> [(Int, Int, Int)] -> [(Int, Int, Int)]
parseTrippleSwapBlock [] acc = reverse acc
parseTrippleSwapBlock [_] acc = reverse acc
parseTrippleSwapBlock (x : y : tl) acc = parseTrippleSwapBlock (y : tl) (parseTrippleSwap (x, y) : acc)

part1 :: String -> String
part1 input = show result
  where
    iLines = lines input
    blocks = splitOn [""] iLines
    (freqsStr, swapsStr, targetStr) = case blocks of
      [f, s, t] -> (f, s, t)
      _ -> error "Invalid input format"
    freqs = map read freqsStr :: [Int]
    swaps = map parseSwap swapsStr
    target = case targetStr of
      [t] -> read t - 1 :: Int
      _ -> error "Invalid target format"
    result = doSwap freqs swaps target

part2 :: String -> String
part2 input = show result
  where
    iLines = lines input
    blocks = splitOn [""] iLines
    (freqsStr, swapsStr, targetStr) = case blocks of
      [f, s, t] -> (f, s, t)
      _ -> error "Invalid input format"
    freqs = map read freqsStr :: [Int]
    swaps = parseTrippleSwapBlock (swapsStr ++ take 1 swapsStr) []
    target = case targetStr of
      [t] -> read t - 1 :: Int
      _ -> error "Invalid target format"
    result = doTrippleSwap freqs swaps target

part3 :: String -> String
part3 input = show result
  where
    iLines = lines input
    blocks = splitOn [""] iLines
    (freqsStr, swapsStr, targetStr) = case blocks of
      [f, s, t] -> (f, s, t)
      _ -> error "Invalid input format"
    freqs = map read freqsStr :: [Int]
    swaps = map parseSwap swapsStr
    target = case targetStr of
      [t] -> read t - 1 :: Int
      _ -> error "Invalid target format"
    result = doSwapBlock freqs swaps target

solve :: Int -> String -> String
solve n input = case n of
  1 -> part1 input
  2 -> part2 input
  3 -> part3 input
  _ -> error "Invalid part number"