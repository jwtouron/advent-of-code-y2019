module Lib.Day1
  ( spec
  )
where

import           Data.List
import           Lib.Util

input :: IO [Int]
input = map read . lines <$> readFile "input/day1.txt"

solvePart1 :: [Int] -> Int
solvePart1 = foldl' f 0 where f total x = total + x `div` 3 - 2

solvePart2 :: [Int] -> Int
solvePart2 = foldl' f 0
 where
  f total x = total + g x

  g x = case x `div` 3 - 2 of
    n | n <= 0 -> 0
    n          -> n + g n

spec :: Spec
spec = mkSpec input 1 [flip shouldBe 3235550 . solvePart1, flip shouldBe 4850462 . solvePart2]
