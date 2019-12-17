{-# LANGUAGE RecordWildCards #-}

module Lib.Day3
  ( spec
  )
where

import           Control.Arrow
import           Control.Monad
import           Data.List                      ( foldl'
                                                , minimumBy
                                                )
import           Data.List.Split                ( splitOn )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Ord                       ( comparing )
import           Lib.Util

listToTuple2 :: [a] -> (a, a)
listToTuple2 [x1, x2] = (x1, x2)
listToTuple2 _        = error "listtotuple2: wrong list size"

data Direction
  = U
  | D
  | L
  | R
  deriving (Show)

data Instruction =
  Instr Direction Int
  deriving (Show)

parseInstruction :: String -> Instruction
parseInstruction (c : cs) =
  let dir = case c of
        'U' -> U
        'D' -> D
        'R' -> R
        'L' -> L
        _   -> error $ "Unknown character: " ++ [c]
  in  Instr dir (read cs)

parseInstruction [] = error "Empty instruction"

input :: IO ([Instruction], [Instruction])
input =
  listToTuple2 . map (map parseInstruction . splitOn ",") . lines <$> readFile "input/day3.txt"

example1 :: ([Instruction], [Instruction])
example1 = join (***) (map parseInstruction . splitOn ",") ("R8,U5,L5,D3", "U7,R6,D4,L4")

type Point = (Int, Int)

data Path = Path { pathHead :: Point, pathLength :: Int, pathPoints :: Map Point Int } deriving (Show)

newPath :: Path
newPath = Path (0, 0) 0 (Map.singleton (0, 0) 0)

followInstruction :: Instruction -> Path -> Path
followInstruction (Instr dir dist) path@Path {..} = foldl' f path points
 where
  f Path {..} point =
    let pathLength' = pathLength + 1
    in  Path { pathHead   = point
             , pathLength = pathLength'
             , pathPoints = Map.insert point pathLength' pathPoints
             }
  (x, y) = pathHead
  points = case dir of
    U -> [ (x, y') | y' <- [y + 1 .. y + dist] ]
    D -> [ (x, y') | y' <- [y - 1, y - 2 .. y - dist] ]
    R -> [ (x', y) | x' <- [x + 1 .. x + dist] ]
    L -> [ (x', y) | x' <- [x - 1, x - 2 .. x - dist] ]

followInstructions :: [Instruction] -> Path -> Path
followInstructions is path = foldl' (flip followInstruction) path is

pathIntersections :: Path -> Path -> Map Point Int
pathIntersections path1 path2 =
  Map.delete (0, 0)
    . uncurry (Map.intersectionWith (+))
    . listToTuple2
    . map pathPoints
    $ [path1, path2]

instructionIntersections :: [Instruction] -> [Instruction] -> Map Point Int
instructionIntersections is1 is2 =
  uncurry pathIntersections . listToTuple2 . map (`followInstructions` newPath) $ [is1, is2]

solvePart1 :: ([Instruction], [Instruction]) -> Int
solvePart1 iss = minimum . map (manhattenDistance (0, 0)) . Set.elems $ intersections
 where
  intersections :: Set Point
  intersections = Map.keysSet $ uncurry instructionIntersections iss

solvePart2 :: ([Instruction], [Instruction]) -> Int
solvePart2 iss = snd $ minimumBy (comparing snd) $ Map.assocs intersections
  where intersections = uncurry instructionIntersections iss

spec :: Spec
spec = mkSpec input 3 [flip shouldBe 293 . solvePart1, flip shouldBe 27306 . solvePart2]
