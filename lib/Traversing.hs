module Traversing (solve) where

import Data.HashMap.Strict qualified as HM
import Data.Set qualified as Set
import Data.Text qualified as T

type Graph = HM.HashMap T.Text [T.Text]

insertInGraph :: (T.Text, T.Text) -> Graph -> Graph
insertInGraph (key, val) graph = case HM.lookup key graph of
  Just neighbors -> HM.insert key (val : neighbors) graph
  Nothing -> HM.insert key [val] graph

parseLine :: String -> (T.Text, T.Text)
parseLine line = case words line of
  (a : _ : c : _) -> (T.pack a, T.pack c)
  _ -> error "Invalid input format"

part1 :: String -> String
part1 input = show $ Set.size oset
  where
    pairs = map parseLine (lines input)
    oset = foldl' (\acc s -> Set.insert (snd s) $ Set.insert (fst s) acc) Set.empty pairs

part2 :: String -> String
part2 input = res
  where
    pairs = map parseLine (lines input)
    graph = foldr insertInGraph HM.empty pairs
    res = show graph

solve :: Int -> String -> String
solve n input = case n of
  1 -> part1 input
  2 -> part2 input
  _ -> error "Invalid part number"