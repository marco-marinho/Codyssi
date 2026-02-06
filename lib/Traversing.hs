module Traversing (solve) where

import Data.HashMap.Strict qualified as HM
import Data.Set qualified as Set
import Data.Text qualified as T

type Graph = HM.HashMap T.Text [T.Text]

type CitySet = Set.Set T.Text

insertInGraph :: (T.Text, T.Text) -> Graph -> Graph
insertInGraph (key, val) graph = gp2
  where
    gp1 = case HM.lookup key graph of
      Just neighbors -> HM.insert key (val : neighbors) graph
      Nothing -> HM.insert key [val] graph
    gp2 = case HM.lookup val graph of
      Just neighbors -> HM.insert val (key : neighbors) gp1
      Nothing -> HM.insert val [key] gp1

parseLine :: String -> (T.Text, T.Text)
parseLine line = case words line of
  (a : _ : c : _) -> (T.pack a, T.pack c)
  _ -> error "Invalid input format"

countReachable :: Graph -> [T.Text] -> Int -> CitySet
countReachable _ _ (-1) = Set.empty
countReachable graph toVisit depth = Set.fromList toVisit `Set.union` countReachable graph nextToVisit (depth - 1)
  where
    nextToVisit = concatMap (\node -> HM.lookupDefault [] node graph) toVisit

countHours :: Graph -> CitySet -> CitySet -> Int -> Int
countHours _ toVisit _ _ | Set.null toVisit = 0
countHours graph toVisit visited depth = res
  where
    levelWeight = depth * Set.size toVisit
    nextToVisit = Set.fromList $ concatMap (\node -> HM.lookupDefault [] node graph) toVisit
    newVisited = Set.union visited toVisit
    filteredNext = Set.difference nextToVisit newVisited
    res = levelWeight + countHours graph filteredNext newVisited (depth + 1)

part1 :: String -> String
part1 input = show $ Set.size oset
  where
    pairs = map parseLine (lines input)
    oset = foldl' (\acc s -> Set.insert (snd s) $ Set.insert (fst s) acc) Set.empty pairs

part2 :: String -> String
part2 input = show $ Set.size res
  where
    pairs = map parseLine (lines input)
    graph = foldr insertInGraph HM.empty pairs
    res = countReachable graph [T.pack "STT"] 3

part3 :: String -> String
part3 input = show res
  where
    pairs = map parseLine (lines input)
    graph = foldr insertInGraph HM.empty pairs
    res = countHours graph (Set.fromList [T.pack "STT"]) Set.empty 0

solve :: Int -> String -> String
solve n input = case n of
  1 -> part1 input
  2 -> part2 input
  3 -> part3 input
  _ -> error "Invalid part number"