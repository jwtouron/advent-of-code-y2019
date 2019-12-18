module Lib.Day9
  ( spec
  )
where

import           Control.Lens                   ( (^.) )
import           Data.List.Split                ( splitOn )
import qualified Data.Queue                    as Queue
import           Lib.Intcode
import           Lib.Util

input :: IO [Int]
input = map read . splitOn "," <$> readFile "input/day9.txt"

solvePart1 :: [Int] -> Int
solvePart1 program = Queue.back $ runUntilHalted (newMachine program (InputV2 [1])) ^. outputs

solvePart2 :: [Int] -> Int
solvePart2 program = Queue.front $ runUntilHalted (newMachine program (InputV2 [2])) ^. outputs

spec :: Spec
spec = mkSpec input 9 [flip shouldBe 2745604242 . solvePart1, flip shouldBe 51135 . solvePart2]
