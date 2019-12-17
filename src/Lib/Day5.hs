module Lib.Day5
  ( spec
  )
where

import           Control.Lens                   ( (^.) )
import qualified Data.Queue                    as Queue
import           Lib.Intcode
import           Lib.Util
import           Data.List.Split                ( splitOn )

input :: IO [Int]
input = map read . splitOn "," <$> readFile "input/day5.txt"

solvePart1 :: [Int] -> Int
solvePart1 program = Queue.back $ runUntilHalted (newMachine program (InputV2 [1])) ^. outputs

solvePart2 :: [Int] -> Int
solvePart2 program = Queue.front $ runUntilHalted (newMachine program (InputV2 [5])) ^. outputs

spec :: Spec
spec = mkSpec input 5 [flip shouldBe 3122865 . solvePart1, flip shouldBe 773660 . solvePart2]
