module Lib.Day9
  ( solve
  )
where

import           Control.Lens                   ( (^.) )
import           Data.List.Split                ( splitOn )
import           Lib.Intcode
import           Lib.Util                       ( assert' )

input :: IO [Int]
input = map read . splitOn "," <$> readFile "input/day9.txt"

solvePart1 :: [Int] -> Int
solvePart1 program =
  head $ runUntilHalted (newMachineWithSize 1000 program (InputV2 [1])) ^. outputs

solvePart2 :: [Int] -> Int
solvePart2 program =
  head $ runUntilHalted (newMachineWithSize 1000 program (InputV2 [2])) ^. outputs

solve :: IO ()
solve =
  input >>= mapM_ print . sequence [assert' 2745604242 . solvePart1, assert' 51135 . solvePart2]
