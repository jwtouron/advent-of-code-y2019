module Main where

import           Lib.Day1                      as Day1
import           Lib.Day2                      as Day2
import           Lib.Day3                      as Day3
import           Lib.Day4                      as Day4
import           Lib.Day5                      as Day5
import           Lib.Day6                      as Day6
import           Lib.Day7                      as Day7
import           Lib.Day8                      as Day8
import           Lib.Day9                      as Day9
import           System.Environment

allSolutions :: [(Int, IO ())]
allSolutions =
  [ (1, Day1.solve)
  , (2, Day2.solve)
  , (3, Day3.solve)
  , (4, Day4.solve)
  , (5, Day5.solve)
  , (6, Day6.solve)
  , (7, Day7.solve)
  , (8, Day8.solve)
  , (9, Day9.solve)
  ]

main :: IO ()
main = do
  [day] <- getArgs
  if day == "all"
    then mapM_ (\(n, io) -> putStrLn ("Day " ++ show n) >> io) allSolutions
    else snd $ allSolutions !! (read day - 1)
