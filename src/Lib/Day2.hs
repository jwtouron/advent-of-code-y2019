module Lib.Day2
  ( solve
  ) where

import           Control.Monad.ST            (ST, runST)
import           Data.List.Split
import           Data.Vector.Unboxed         (Vector)
import qualified Data.Vector.Unboxed         as V
import           Data.Vector.Unboxed.Mutable (STVector)
import qualified Data.Vector.Unboxed.Mutable as MV
import           Lib.Util

type Dest = Int

type Program = Vector Int

input :: IO Program
input = V.fromList . map read . splitOn "," <$> readFile "input/day2.txt"

op ::
     (Int -> Int -> Int)
  -> STVector s Int
  -> Int
  -> Int
  -> Dest
  -> ST s (STVector s Int)
op f vec x y dest = do
  xIdx <- MV.unsafeRead vec x
  xVal <- MV.unsafeRead vec xIdx
  yIdx <- MV.unsafeRead vec y
  yVal <- MV.unsafeRead vec yIdx
  destIdx <- MV.unsafeRead vec dest
  MV.unsafeWrite vec destIdx (f xVal yVal)
  return vec

add :: STVector s Int -> Int -> Int -> Dest -> ST s (STVector s Int)
add = op (+)

mul :: STVector s Int -> Int -> Int -> Dest -> ST s (STVector s Int)
mul = op (*)

runProgram :: Int -> Int -> Program -> Program
runProgram noun verb program =
  runST $ do
    program' <- V.thaw program
    MV.unsafeWrite program' 1 noun
    MV.unsafeWrite program' 2 verb
    go 0 program'
    V.unsafeFreeze program'
  where
    go :: Int -> STVector s Int -> ST s (STVector s Int)
    go idx program = do
      val <- MV.read program idx
      case val of
        1 -> do
          add program (idx + 1) (idx + 2) (idx + 3)
          go (idx + 4) program
        2 -> do
          mul program (idx + 1) (idx + 2) (idx + 3)
          go (idx + 4) program
        99 -> return program

solvePart1 :: Program -> Int
solvePart1 program = runProgram 12 2 program V.! 0

solvePart2 :: Program -> Int
solvePart2 program =
  let (x, y) =
        head
          [ (x, y)
          | x <- [0 .. 99]
          , y <- [0 .. 99]
          , let z = runProgram x y program V.! 0
          , z == 19690720
          ]
   in 100 * x + y

solve :: IO ()
solve =
  input >>=
  mapM_ print .
  sequence [assert' 6627023 . solvePart1, assert' 4019 . solvePart2]
