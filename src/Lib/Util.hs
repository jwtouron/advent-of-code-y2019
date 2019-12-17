module Lib.Util
  ( manhattenDistance
  , mkSpec
  , module Test.Hspec
  )
where

import           Test.Hspec

manhattenDistance :: (Int, Int) -> (Int, Int) -> Int
manhattenDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

mkSpec :: IO a -> Int -> [a -> Expectation] -> Spec
mkSpec input day specs =
  before input
    $ describe ("Day " ++ show day)
    $ mapM_ (\(i, spec) -> it ("Part " ++ show i) $ \input' -> spec input')
    $ zip [1 :: Int ..] specs
