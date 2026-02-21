module Guards (solve) where

import Data.HashMap.Strict qualified as HM
import Data.List
import Data.Ord (Down (..))
import Data.OrdPSQ qualified as PSQ
import Data.Set qualified as Set

type Connections = HM.HashMap String [(String, Int)]

type PQ = PSQ.OrdPSQ String Int ()

type Seen = Set.Set String

updatePQ :: PQ -> Int -> [(String, Int)] -> PQ
updatePQ pq currDist = foldl' relax pq
  where
    relax acc (node, weight) =
      let newDist = currDist + weight
          f Nothing = ((), Just (newDist, ()))
          f (Just (oldDist, _)) = ((), Just (min oldDist newDist, ()))
       in snd $ PSQ.alter f node acc

dijkstras :: PQ -> Seen -> Connections -> String -> Int
dijkstras pq visited conn target = case PSQ.minView pq of
  Just (currNode, currDist, _, restPQ)
    | Set.member currNode visited -> dijkstras restPQ visited conn target
    | currNode == target -> currDist
    | otherwise -> case HM.lookup currNode conn of
        Just neighbors -> dijkstras (updatePQ restPQ currDist neighbors) (Set.insert currNode visited) conn target
        Nothing -> dijkstras restPQ (Set.insert currNode visited) conn target
  Nothing -> -1

parseLine :: String -> (String, String, Int)
parseLine line = case words line of
  [a, "->", b, "|", c] -> (a, b, read c)
  _ -> error "Invalid input format"

parseLine1 :: String -> (String, String, Int)
parseLine1 line = case words line of
  [a, "->", b, "|", _] -> (a, b, 1)
  _ -> error "Invalid input format"

insertConnection :: Connections -> (String, String, Int) -> Connections
insertConnection conn (a, b, c) = case HM.lookup a conn of
  Just lst -> HM.insert a ((b, c) : lst) conn
  Nothing -> HM.insert a [(b, c)] conn

part1and2 :: String -> (String -> (String, String, Int)) -> String
part1and2 input parser = show res
  where
    connectionTriples = map parser (lines input)
    connections = foldl insertConnection HM.empty connectionTriples
    initPQ = PSQ.insert "STT" 0 () PSQ.empty
    allNodes = Set.fromList $ concatMap (\(a, b, _) -> [a, b]) connectionTriples
    allCosts = foldl' (\acc node -> dijkstras initPQ Set.empty connections node : acc) [] (Set.toList allNodes)
    sortedCosts = sortOn Down allCosts
    res = product $ take 3 sortedCosts

part1 :: String -> String
part1 input = part1and2 input parseLine1

part2 :: String -> String
part2 input = part1and2 input parseLine

part3 :: String -> String
part3 input = input

solve :: Int -> String -> String
solve n input = case n of
  1 -> part1 input
  2 -> part2 input
  3 -> part3 input
  _ -> error "Invalid part number"