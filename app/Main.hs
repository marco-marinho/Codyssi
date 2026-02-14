module Main where

import Arithmetic qualified as A
import Budget qualified as BG
import Compass qualified as C
import Readings qualified as R
import Sensors qualified as S
import System.Environment (getArgs)
import Traversing qualified as T

main :: IO ()
main = do
  args <- getArgs
  case args of
    (name : part : _) | Just action <- lookup name dispatch -> action part
    _ -> putStrLn "Usage: <problem> <part>"

solve :: String -> (Int -> String -> String) -> String -> IO ()
solve name f part = readFile ("data/" ++ name ++ ".txt") >>= putStrLn . f (read part :: Int)

dispatch :: [(String, String -> IO ())]
dispatch =
  [ ("budget", solve "budget" BG.solve),
    ("sensors", solve "sensors" S.solve),
    ("readings", solve "readings" R.solve),
    ("traversing", solve "traversing" T.solve),
    ("compass", solve "compass" C.solve),
    ("arithmetic", solve "arithmetic" A.solve)
  ]