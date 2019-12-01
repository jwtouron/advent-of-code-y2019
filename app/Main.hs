module Main where

import Lib.Day1 as Day1
import System.Environment

main :: IO ()
main = do
  [day] <- fmap (map read) getArgs
  case day of
    1 -> Day1.solve
