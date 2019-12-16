module Lib.Util
  ( assert'
  , manhattenDistance
  )
where

import           Control.Exception              ( assert )

assert' :: Eq a => a -> a -> a
assert' expected actual = assert (expected == actual) actual

manhattenDistance :: (Int, Int) -> (Int, Int) -> Int
manhattenDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
