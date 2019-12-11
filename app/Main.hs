module Main where

import           Lib.Day1                      as Day1
import           Lib.Day2                      as Day2
import           Lib.Day3                      as Day3
import           Lib.Day4                      as Day4
import           Lib.Day5                      as Day5
import           System.Environment

main :: IO ()
main = do
  [day] <- fmap (map read) getArgs
  case day of
    1 -> Day1.solve
    2 -> Day2.solve
    3 -> Day3.solve
    4 -> Day4.solve
    5 -> Day5.solve
