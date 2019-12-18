module Lib.Day11
  ( spec
  , printPart2Solution
  )
where

import           Control.Lens
import           Data.Foldable                  ( toList )
import           Data.List                      ( foldl' )
import           Data.List.Split                ( splitOn )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import qualified Data.Queue                    as Queue
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Lib.Intcode
import           Lib.Util

input :: IO [Int]
input = map read . splitOn "," <$> readFile "input/day11.txt"

data Direction = N | S | E | W deriving (Show)

data Robot =
  Robot { direction :: Direction
        , location :: (Int,Int)
        , panels :: Map (Int,Int) Int
        , painted :: Set (Int,Int)
        } deriving (Show)

newRobot :: Robot
newRobot = Robot N (0, 0) Map.empty Set.empty

leftTurn :: Direction -> Direction
leftTurn N = W
leftTurn W = S
leftTurn S = E
leftTurn E = N

rightTurn :: Direction -> Direction
rightTurn N = E
rightTurn E = S
rightTurn S = W
rightTurn W = N

moveRobot :: Int -> Robot -> Robot
moveRobot turn robot = robot { direction = direction', location = location' }
 where
  (x, y)     = location robot
  direction' = if turn == 0 then leftTurn (direction robot) else rightTurn (direction robot)
  location'  = case direction' of
    N -> (x, y + 1)
    S -> (x, y - 1)
    E -> (x + 1, y)
    W -> (x - 1, y)

paintPanel :: Int -> Robot -> Robot
paintPanel color robot = robot { panels  = Map.insert (location robot) color (panels robot)
                               , painted = Set.insert (location robot) (painted robot)
                               }

has2Outputs :: Machine -> Bool
has2Outputs machine = Queue.length (machine ^. outputs) == 2

runRobot :: Int -> [Int] -> Robot
runRobot input program = go robot machine
 where
  go robot machine
    | isHalted machine
    = robot
    | has2Outputs machine
    = let [color, turn] = toList $ machine ^. outputs
          robot'        = moveRobot turn $ paintPanel color robot
          machine'      = machine & outputs .~ Queue.empty & inputs %~ Queue.push
            (Map.findWithDefault 0 (location robot') (panels robot'))
      in  go robot' machine'
    | otherwise
    = go robot (runUntil ((||) <$> has2Outputs <*> isHalted) machine)
  machine = newMachine program [input]
  robot   = newRobot

solvePart1 :: [Int] -> Int
solvePart1 = Set.size . painted . runRobot 0

solvePart2 :: [Int] -> String
solvePart2 program = concatMap (\y -> map (\x -> getChar x y) [minX .. maxX] ++ "\n")
                               [maxY, maxY - 1 .. minY - 1]
 where
  panels'                  = panels $ runRobot 1 program
  (minX, maxX, minY, maxY) = foldl' f (999999, -999999, 999999, -99999) $ Map.assocs panels'
  f (minX, maxX, minY, maxY) ((x, y), _) = case () of
    _ | x < minX -> (x, maxX, minY, maxY)
    _ | x > maxX -> (minX, x, minY, maxY)
    _ | y < minY -> (minX, maxX, y, maxY)
    _ | y > maxY -> (minX, maxX, minY, y)
    _            -> (minX, maxX, minY, maxY)
  getChar x y = if Map.findWithDefault 0 (x, y) panels' == 0 then ' ' else 'X'

printPart2Solution :: IO ()
printPart2Solution = input >>= putStrLn . solvePart2
-- >>> printPart2Solution
--  XXXX  XX  XXX  XXX   XX  XXXX   XX X  X   
--  X    X  X X  X X  X X  X X       X X X    
--  XXX  X  X X  X XXX  X    XXX     X XX     
--  X    XXXX XXX  X  X X    X       X X X    
--  X    X  X X X  X  X X  X X    X  X X X    
--  X    X  X X  X XXX   XX  X     XX  X  X

-- FARBCFJK

spec :: Spec
spec = mkSpec input 11 [(`shouldBe` 2082) . solvePart1]
