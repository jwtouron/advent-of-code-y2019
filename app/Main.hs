module Main where

import           Test.Hspec                     ( hspec )
import           Lib.Day1                      as Day1
import           Lib.Day2                      as Day2
import           Lib.Day3                      as Day3
import           Lib.Day4                      as Day4
import           Lib.Day5                      as Day5
import           Lib.Day6                      as Day6
import           Lib.Day7                      as Day7
import           Lib.Day8                      as Day8
import           Lib.Day9                      as Day9
import           Lib.Day10                     as Day10
import           Lib.Day11                     as Day11
import           Lib.Day12                     as Day12
import           Lib.Day13                     as Day13
import           Lib.Day14                     as Day14
import           Lib.Day15                     as Day15
import           Lib.Day16                     as Day16
import           Lib.Day17                     as Day17
import           Lib.Day18                     as Day18

main :: IO ()
main = do
  Day18.input >>= putStrLn . show . Day18.solvePart1
  Day18.input >>= putStrLn . show . Day18.solvePart2 --
--  print $ Day18.solvePart1''
  return ()

  -- hspec $ do
  --   Day1.spec
  --   Day2.spec
  --   Day3.spec
  --   Day4.spec
  --   Day5.spec
  --   Day6.spec
  --   Day7.spec
  --   Day8.spec
  --   Day9.spec
  --   Day10.spec
  --   Day11.spec
  --   Day12.spec
  --   Day13.spec
  --   Day14.spec
  --   Day15.spec
  --   Day16.spec
  --   Day17.spec
