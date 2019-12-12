module Lib.Day5
  ( solve
  )
where

import           Control.Lens                   ( (^.) )
import           Lib.Intcode
import           Lib.Util                       ( assert' )
import           Data.List.Split                ( splitOn )

input :: IO [Int]
input = map read . splitOn "," <$> readFile "input/day5.txt"

solvePart1 :: [Int] -> Int
solvePart1 program = head $ runUntilHalted (newMachine program (InputV2 [1])) ^. outputs

solvePart2 :: [Int] -> Int
solvePart2 program = head $ runUntilHalted (newMachine program (InputV2 [5])) ^. outputs

solve :: IO ()
solve =
  input >>= mapM_ print . sequence [assert' 3122865 . solvePart1, assert' 773660 . solvePart2]
