module Lib.Day2
  ( spec
  )
where

import           Control.Lens                   ( (^.) )
import qualified Data.IntMap                   as IntMap
import           Data.List.Split
import           Lib.Intcode
import           Lib.Util

input :: IO [Int]
input = map read . splitOn "," <$> readFile "input/day2.txt"

solvePart1 :: [Int] -> Int
solvePart1 program = (runUntilHalted (newMachine program (InputV1 12 2)) ^. memory) IntMap.! 0

solvePart2 :: [Int] -> Int
solvePart2 program =
  let xy = head
        [ (x, y)
        | x <- [0 .. 99]
        , y <- [0 .. 99]
        , let z = (runUntilHalted (newMachine program (InputV1 x y)) ^. memory) IntMap.! 0
        , z == 19690720
        ]
  in  100 * fst xy + snd xy

spec :: Spec
spec = mkSpec input 2 [flip shouldBe 6627023 . solvePart1, flip shouldBe 4019 . solvePart2]
