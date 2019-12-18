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

addInputs :: Int -> Int -> [Int] -> [Int]
addInputs noun verb (x : _ : _ : xs) = x : noun : verb : xs

solvePart1 :: [Int] -> Int
solvePart1 program = (runUntilHalted (newMachine (addInputs 12 2 program) []) ^. memory) IntMap.! 0

solvePart2 :: [Int] -> Int
solvePart2 program =
  let xy = head
        [ (x, y)
        | x <- [0 .. 99]
        , y <- [0 .. 99]
        , let z = (runUntilHalted (newMachine (addInputs x y program) []) ^. memory) IntMap.! 0
        , z == 19690720
        ]
  in  100 * fst xy + snd xy

spec :: Spec
spec = mkSpec input 2 [flip shouldBe 6627023 . solvePart1, flip shouldBe 4019 . solvePart2]
