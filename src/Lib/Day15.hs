{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib.Day15
  ( spec
  )
where

import           Algorithm.Search               ( bfsM )
import           Control.Lens                   ( (&)
                                                , (%~)
                                                , (^.)
                                                , (.~)
                                                , makeFieldsNoPrefix
                                                )
import           Control.Monad                  ( guard )
import           Control.Monad.State.Strict     ( State
                                                , modify'
                                                , evalState
                                                , execState
                                                )
import           Data.List.Split                ( splitOn )
import           Data.Maybe                     ( fromJust )
import qualified Data.Queue                    as Queue
import           Lib.Intcode
import           Lib.Util

input :: IO [Int]
input = map read . splitOn "," <$> readFile "input/day15.txt"

type Location = (Int, Int)

data LocType = Wall | Open | OxygenSystem deriving (Eq,Ord,Show)

data ProblemState =
  State { _robotLoc :: Location
        , _machine :: Machine
        , _atOxygenSystem :: Bool
        , _travelled :: Int
        } deriving (Show)

makeFieldsNoPrefix ''ProblemState

instance Eq ProblemState where
  s1 == s2 = (s1 ^. robotLoc) == (s2 ^. robotLoc)

instance Ord ProblemState where
  s1 <= s2 = (s1 ^. robotLoc) <= (s2 ^. robotLoc)

newProblemState :: [Int] -> ProblemState
newProblemState program = State (0, 0) (newMachine program []) False 0

neighbors' :: ProblemState -> [ProblemState]
neighbors' state = do
  dir <- [1 .. 4]
  let machine'   = runUntil hasOutput ((state & (machine . inputs) %~ Queue.push dir) ^. machine)
      result     = Queue.front $ machine' ^. outputs
      robotLoc'  = move dir (state ^. robotLoc)
      travelled' = (state & travelled %~ (+ 1)) ^. travelled
  guard $ result /= 0
  return
    $  state
    &  robotLoc
    .~ robotLoc'
    &  machine
    .~ (clearOutputs machine')
    &  atOxygenSystem
    .~ (outputToLocType result == OxygenSystem)
    &  travelled
    .~ travelled'

neighbors :: ProblemState -> State Int [ProblemState]
neighbors state = do
  let ns         = neighbors' state
      travelleds = map (^. travelled) ns
  modify' (\x -> maximum (x : travelleds))
  return ns

hasOutput :: Machine -> Bool
hasOutput machine' = Queue.length (machine' ^. outputs) > 0

move :: Int -> Location -> Location
move dir (x, y) = case dir of
  1 -> (x, y + 1)
  2 -> (x, y - 1)
  3 -> (x - 1, y)
  4 -> (x + 1, y)
  _ -> undefined

outputToLocType :: Int -> LocType
outputToLocType 0 = Wall
outputToLocType 1 = Open
outputToLocType 2 = OxygenSystem
outputToLocType _ = undefined

foundOxygenSystem :: ProblemState -> State Int Bool
foundOxygenSystem state = return $ state ^. atOxygenSystem

solvePart1 :: [Int] -> Int
solvePart1 program =
  length $ fromJust $ evalState (bfsM neighbors foundOxygenSystem (newProblemState program)) 0

foundOxygenSystem2 :: ProblemState -> State Int Bool
foundOxygenSystem2 _ = return False

solvePart2 :: [Int] -> Int
solvePart2 program = execState (bfsM neighbors foundOxygenSystem2 (start & travelled .~ 0)) 0 - 1
 where
  start = head $ reverse $ fromJust $ evalState
    (bfsM neighbors foundOxygenSystem (newProblemState program))
    0

spec :: Spec
spec = mkSpec input 15 [(`shouldBe` 234) . solvePart1, (`shouldBe` 292) . solvePart2]
