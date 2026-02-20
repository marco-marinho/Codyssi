module Main where

import Aeolian qualified
import Arithmetic qualified
import Budget qualified
import Compass qualified
import Cyclops qualified
import Games qualified
import Islands qualified
import Lotus qualified
import Readings qualified
import Risky qualified
import Sensors qualified
import Siren qualified
import Supplies qualified
import System.Environment (getArgs)
import Traversing qualified
import Windy qualified
import Whirlpool qualified

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
  [ ("budget", solve "budget" Budget.solve),
    ("sensors", solve "sensors" Sensors.solve),
    ("readings", solve "readings" Readings.solve),
    ("traversing", solve "traversing" Traversing.solve),
    ("compass", solve "compass" Compass.solve),
    ("arithmetic", solve "arithmetic" Arithmetic.solve),
    ("supplies", solve "supplies" Supplies.solve),
    ("aeolian", solve "aeolian" Aeolian.solve),
    ("islands", solve "islands" Islands.solve),
    ("lotus", solve "lotus" Lotus.solve),
    ("siren", solve "siren" Siren.solve),
    ("risky", solve "risky" Risky.solve),
    ("windy", solve "windy" Windy.solve),
    ("cyclops", solve "cyclops" Cyclops.solve),
    ("games", solve "games" Games.solve),
    ("whirlpool", solve "whirlpool" Whirlpool.solve)
  ]