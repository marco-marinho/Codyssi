module Whirlpool (solve) where

import Control.Monad (forM, forM_)
import Control.Monad.ST
import Data.List.Split
import Data.Vector.Unboxed.Mutable qualified as MV

data Target = Row | Col | All deriving (Eq, Show)

data Instruction = Shift Target Int Int | Multiply Target Int Int | Sub Target Int Int | Add Target Int Int deriving (Eq, Show)

data ControlFlow = Cycle | Act deriving (Eq, Show)

data MutableTable s = MutableTable Int Int (MV.MVector s Int)

wrap :: Int -> Int
wrap x = ((x `mod` 1073741824) + 1073741824) `mod` 1073741824

get :: MutableTable s -> Int -> Int -> ST s Int
get (MutableTable _ cols v) i j = MV.read v (i * cols + j)

put :: MutableTable s -> Int -> Int -> Int -> ST s ()
put (MutableTable _ cols v) i j val = MV.write v (i * cols + j) (wrap val)

arith :: MutableTable s -> Target -> Int -> Int -> (Int -> Int -> Int) -> ST s ()
arith table@(MutableTable rows cols _) target amount identifier op = case target of
  Row -> forM_ [0 .. cols - 1] $ \j -> do
    val <- get table identifier j
    put table identifier j (op val amount)
  Col -> forM_ [0 .. rows - 1] $ \j -> do
    val <- get table j identifier
    put table j identifier (op val amount)
  All -> forM_ [0 .. rows - 1] $ \i ->
    forM_ [0 .. cols - 1] $ \j -> do
      val <- get table i j
      put table i j (op val amount)

shift :: MutableTable s -> Target -> Int -> Int -> ST s ()
shift table@(MutableTable rows cols _) target identifier times = case target of
  Row -> do
    buffer <- MV.new cols
    forM_ [0 .. cols - 1] $ \j -> do
      val <- get table identifier j
      MV.write buffer ((j + times) `mod` cols) val
    forM_ [0 .. cols - 1] $ \j -> do
      val <- MV.read buffer j
      put table identifier j val
  Col -> do
    buffer <- MV.new rows
    forM_ [0 .. rows - 1] $ \i -> do
      val <- get table i identifier
      MV.write buffer ((i + times) `mod` rows) val
    forM_ [0 .. rows - 1] $ \i -> do
      val <- MV.read buffer i
      put table i identifier val
  _ -> error "Invalid target for shift operation"

step :: MutableTable s -> Instruction -> ST s ()
step table instruction = case instruction of
  Multiply target amount times -> arith table target amount times (*)
  Add target amount times -> arith table target amount times (+)
  Sub target amount times -> arith table target amount times (-)
  Shift target identifier times -> shift table target identifier times

rowSums :: MutableTable s -> ST s [Int]
rowSums table@(MutableTable rows cols _) = do
  sums <- MV.new rows
  forM_ [0 .. rows - 1] $ \i -> do
    forM_ [0 .. cols - 1] $ \j -> do
      val <- get table i j
      prev <- MV.read sums i
      MV.write sums i (prev + val)
  forM [0 .. rows - 1] $ \i -> MV.read sums i

colSums :: MutableTable s -> ST s [Int]
colSums table@(MutableTable rows cols _) = do
  sums <- MV.new cols
  forM_ [0 .. cols - 1] $ \j -> do
    forM_ [0 .. rows - 1] $ \i -> do
      val <- get table i j
      prev <- MV.read sums j
      MV.write sums j (prev + val)
  forM [0 .. cols - 1] $ \j -> MV.read sums j

runInstructions :: [[Int]] -> [Instruction] -> Int
runInstructions [] _ = -1
runInstructions arrInit@(first : _) instructions = runST $ do
  let rows = length arrInit
      cols = length first
  v <- MV.new (rows * cols)
  forM_ (zip [0 ..] arrInit) $ \(i, row) ->
    forM_ (zip [0 ..] row) $ \(j, val) ->
      MV.write v (i * cols + j) val
  let table = MutableTable rows cols v
  forM_ instructions $ \instruction -> step table instruction
  rSums <- rowSums table
  cSums <- colSums table
  return $ maximum (rSums ++ cSums)

parseArray :: String -> [[Int]]
parseArray str = map (map read . words) (lines str)

parseInstruction :: String -> Instruction
parseInstruction str = case words str of
  ("SHIFT" : "COL" : number : "BY" : times : _) -> Shift Col (read number - 1) (read times)
  ("SHIFT" : "ROW" : number : "BY" : times : _) -> Shift Row (read number - 1) (read times)
  ("MULTIPLY" : amount : "COL" : times : _) -> Multiply Col (read amount) (read times - 1)
  ("MULTIPLY" : amount : "ROW" : times : _) -> Multiply Row (read amount) (read times - 1)
  ("MULTIPLY" : amount : "ALL" : _) -> Multiply All (read amount) (-1)
  ("ADD" : amount : "COL" : times : _) -> Add Col (read amount) (read times - 1)
  ("ADD" : amount : "ROW" : times : _) -> Add Row (read amount) (read times - 1)
  ("ADD" : amount : "ALL" : _) -> Add All (read amount) (-1)
  ("SUB" : amount : "COL" : times : _) -> Sub Col (read amount) (read times - 1)
  ("SUB" : amount : "ROW" : times : _) -> Sub Row (read amount) (read times - 1)
  ("SUB" : amount : "ALL" : _) -> Sub All (read amount) (-1)
  _ -> error "Invalid instruction format"

doControlFlow :: [ControlFlow] -> [Instruction] -> [Instruction] -> [Instruction]
doControlFlow _ [] acc = reverse acc
doControlFlow [] _ acc = reverse acc
doControlFlow (cf : cfs) (i : is) acc = case cf of
  Cycle -> doControlFlow cfs (is ++ [i]) acc
  Act -> doControlFlow cfs is (i : acc)

parseControlFlow :: String -> ControlFlow
parseControlFlow str = case str of
  "CYCLE" -> Cycle
  "ACT" -> Act
  _ -> error "Invalid control flow instruction"

parse :: String -> ([[Int]], [Instruction], [ControlFlow])
parse input = (arr, instructions, controlFlow)
  where
    iLines = lines input
    blocks = splitOn [""] iLines
    (arrStr, instStr, condStr) = case blocks of
      [p1, p2, p3] -> (p1, p2, p3)
      _ -> error "Invalid input format"
    arr = parseArray (unlines arrStr)
    instructions = map parseInstruction instStr
    controlFlow = map parseControlFlow (filter (/= "TAKE") condStr)

part1 :: String -> String
part1 input = show res
  where
    (arr, instructions, _) = parse input
    res = runInstructions arr instructions

part2 :: String -> String
part2 input = show res
  where
    (arr, instructions, controlFlow) = parse input
    finalInstructions = doControlFlow controlFlow instructions []
    res = runInstructions arr finalInstructions

part3 :: String -> String
part3 input = show res
  where
    (arr, instructions, controlFlow) = parse input
    finalInstructions = doControlFlow (cycle controlFlow) instructions []
    res = runInstructions arr finalInstructions

solve :: Int -> String -> String
solve n input = case n of
  1 -> part1 input
  2 -> part2 input
  3 -> part3 input
  _ -> error "Invalid part number"