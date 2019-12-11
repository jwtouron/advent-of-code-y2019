module Lib.Day2
  ( solve
  )
where

import           Control.Lens                   ( (^.) )
import qualified Data.Vector.Unboxed           as V
import           Data.List.Split
import           Lib.Intcode
import           Lib.Util

input :: IO [Int]
input = map read . splitOn "," <$> readFile "input/day2.txt"

solvePart1 :: [Int] -> Int
solvePart1 program = (runProgram program (InputV1 12 2) ^. memory) V.! 0

solvePart2 :: [Int] -> Int
solvePart2 program =
  let xy = head
        [ (x, y)
        | x <- [0 .. 99]
        , y <- [0 .. 99]
        , let z = (runProgram program (InputV1 x y) ^. memory) V.! 0
        , z == 19690720
        ]
  in  100 * fst xy + snd xy

solve :: IO ()
solve = input >>= mapM_ print . sequence [assert' 6627023 . solvePart1, assert' 4019 . solvePart2]
