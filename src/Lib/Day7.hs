module Lib.Day7
  ( spec
  )
where

import           Control.Lens                   ( (^.)
                                                , (&)
                                                , (%~)
                                                )
import           Control.Lens.Tuple
import           Data.Foldable                  ( toList )
import           Data.List                      ( foldl'
                                                , permutations
                                                )
import           Data.List.Split                ( splitOn )
import qualified Data.Queue                    as Queue
import qualified Data.IntMap                   as IntMap
import           Lib.Intcode
import           Lib.Util

input :: IO [Int]
input = map read . splitOn "," <$> readFile "input/day7.txt"

solvePart1 :: [Int] -> Int
solvePart1 program = maximum $ map f (permutations [0 .. 4])
 where
  f = head . foldl' g [0]
  g outs x = toList $ runUntilHalted (newMachine program (x : outs)) ^. outputs

needsInput :: Machine -> Bool
needsInput machine =
  let opcode = ((machine ^. memory) IntMap.! (machine ^. instrPtr)) `mod` 100
  in  opcode == 3 && Queue.null (machine ^. inputs)

solvePart2 :: [Int] -> Int
solvePart2 program = maximum
  $ map (loop 0 . map (\p -> newMachine program [p])) (permutations [5 .. 9])
 where
  loop :: Int -> [Machine] -> Int
  loop seed machines =
    let (machines'@[_, _, _, _, machine'], seed') = runMachines (machines, seed)
    in  if isHalted machine' then Queue.back $ machine' ^. outputs else loop seed' machines'
  runMachines :: ([Machine], Int) -> ([Machine], Int)
  runMachines (machines, seed) = foldl' f ([], seed) machines & _1 %~ reverse
  f :: ([Machine], Int) -> Machine -> ([Machine], Int)
  f (machines, seed) machine =
    let machine'  = runUntil shouldStop (machine & inputs %~ Queue.push seed)
        machines' = machine' : machines
        out       = Queue.back $ machine' ^. outputs
    in  (machines', out)
  shouldStop :: Machine -> Bool
  shouldStop = (||) <$> isHalted <*> needsInput

spec :: Spec
spec = mkSpec input 7 [flip shouldBe 13848 . solvePart1, flip shouldBe 12932154 . solvePart2]
