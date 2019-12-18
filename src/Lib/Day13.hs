module Lib.Day13
  ( spec
  )
where

import           Control.Lens
import           Data.Foldable                  ( toList )
import           Data.List.Split                ( splitOn )
import qualified Data.Set                      as Set
import qualified Data.Queue                    as Queue
import qualified Data.IntMap                   as IntMap
import           Lib.Intcode
import           Lib.Util

input :: IO [Int]
input = map read . splitOn "," <$> readFile "input/day13.txt"

has3Outputs :: Machine -> Bool
has3Outputs machine = Queue.length (machine ^. outputs) == 3

solvePart1 :: [Int] -> Int
solvePart1 program = Set.size $ go (newMachine program (InputV2 [])) Set.empty
 where
  go machine tiles
    | isHalted machine
    = tiles
    | has3Outputs machine
    = let [x, y, id] = toList $ machine ^. outputs
          machine'   = clearOutputs machine
          tiles'     = if id == 2 then Set.insert (x, y) tiles else tiles
      in  go machine' tiles'
    | otherwise
    = go (runUntil ((||) <$> has3Outputs <*> isHalted) machine) tiles

needsInput :: Machine -> Bool
needsInput machine =
  let opcode = (machine ^. memory) IntMap.! (machine ^. instrPtr) `mod` 100
  in  opcode == 3 && Queue.null (machine ^. inputs)

data Game =
  Game { ball :: Maybe Int
       , paddle :: Maybe Int
       , score :: Int
       } deriving (Show)

solvePart2 :: [Int] -> Int
solvePart2 program = go (Game Nothing Nothing 0) (newMachine (2 : tail program) (InputV2 []))
 where
  go game machine
    | isHalted machine
    = score game
    | needsInput machine
    = let input = case (ball game, paddle game) of
            (Just b, Just p) | p < b -> 1
            (Just b, Just p) | p > b -> -1
            _                        -> 0
      in  go game (machine & inputs %~ Queue.push input)
    | has3Outputs machine
    = let [x, y, id] = toList $ machine ^. outputs
          game'      = case (x, y, id) of
            (-1, 0, x) -> game { score = x }
            (x , _, 3) -> game { paddle = Just x }
            (x , _, 4) -> game { ball = Just x }
            _          -> game
          machine' = clearOutputs machine
      in  go game' machine'
    | otherwise
    = go game (runUntil (\m -> needsInput m || has3Outputs m || isHalted m) machine)

spec :: Spec
spec = mkSpec input 13 [(`shouldBe` 253) . solvePart1, (`shouldBe` 12263) . solvePart2]
