module Lib.Day7
  ( solve
  )
where

import           Control.Lens                   ( (^.)
                                                , (&)
                                                , (%~)
                                                )
import           Control.Lens.Tuple
import           Data.List                      ( foldl'
                                                , permutations
                                                )
import           Data.List.Split                ( splitOn )
import qualified Data.Vector.Unboxed           as V
import           Lib.Intcode
import           Lib.Util                       ( assert' )

input :: IO [Int]
input = map read . splitOn "," <$> readFile "input/day7.txt"

solvePart1 :: [Int] -> Int
solvePart1 program = maximum $ map f (permutations [0 .. 4])
 where
  f = head . foldl' g [0]
  g outs x = runUntilHalted (newMachine program (InputV2 (x : outs))) ^. outputs

solvePart2 :: [Int] -> Int
solvePart2 program = maximum
  $ map (loop 0 . map (\p -> newMachine program (InputV2 [p]))) (permutations [5 .. 9])
 where
  loop :: Int -> [Machine] -> Int
  loop seed machines =
    let (machines'@[_, _, _, _, machine'], out) = runMachines (machines, seed)
    in  if isHalted machine' then head $ machine' ^. outputs else loop out machines'
  runMachines :: ([Machine], Int) -> ([Machine], Int)
  runMachines (machines, seed) = foldl' f ([], seed) machines & _1 %~ reverse
  f :: ([Machine], Int) -> Machine -> ([Machine], Int)
  f (machines, seed) machine =
    let machine'  = runUntil shouldStop (machine & inputs %~ (++ [seed]))
        machines' = machine' : machines
        out       = head $ machine' ^. outputs
    in  (machines', out)
  needsInput :: Machine -> Bool
  needsInput machine =
    let opcode = ((machine ^. memory) V.! (machine ^. instrPtr)) `mod` 100
    in  opcode == 3 && null (machine ^. inputs)
  shouldStop :: Machine -> Bool
  shouldStop = (||) <$> isHalted <*> needsInput

solve :: IO ()
solve =
  input >>= mapM_ print . sequence [assert' 13848 . solvePart1, assert' 12932154 . solvePart2]
