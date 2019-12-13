module Lib.Day8
  ( solve
  )
where

import           Control.Lens                   ( (%~)
                                                , (&)
                                                , (^.)
                                                )
import           Control.Lens.Tuple
import           Control.Monad                  ( forM_ )
import           Data.Char                      ( digitToInt )
import           Data.List                      ( foldl'
                                                , minimumBy
                                                )
import qualified Data.IntMap.Lazy              as IntMap
import           Data.Ord                       ( comparing )
import           Lib.Util                       ( assert' )

input :: IO [Int]
input = map digitToInt . init <$> readFile "input/day8.txt"

splitMany :: Int -> [a] -> [[a]]
splitMany n xs = reverse $ go n xs []
 where
  go n xs acc = case splitAt n xs of
    (as, []) -> as : acc
    (as, bs) -> go n bs (as : acc)

solvePart1 :: [Int] -> Int
solvePart1 xs = (layer ^. _2) * (layer ^. _3)
 where
  layer       = minimumBy (comparing (^. _1)) $ map countDigits layers
  layers      = splitMany (25 * 6) xs
  countDigits = foldl' f (0, 0, 0)
  f :: (Int, Int, Int) -> Int -> (Int, Int, Int)
  f counts 0 = counts & _1 %~ (+ 1)
  f counts 1 = counts & _2 %~ (+ 1)
  f counts 2 = counts & _3 %~ (+ 1)
  f _      _ = error "f"

solvePart2 :: [Int] -> IO ()
solvePart2 xs = do
  forM_ [0 .. 5] $ \y -> do
    forM_ [0 .. 24] $ \x -> putChar $ calcChar x y
    putStrLn ""
 where
  layers   = reverse $ splitMany (25 * 6) xs
  pixelMap = foldl'
    (\m l -> foldl' (\m (idx, pixel) -> IntMap.insertWith (++) idx [pixel] m) m $ zip [0 ..] l)
    IntMap.empty
    layers
  calcChar x y =
    let choices = pixelMap IntMap.! (y * 25 + x)
    in  if head (dropWhile (== 2) choices) == 0 then ' ' else 'X'

solve :: IO ()
solve = do
  input >>= print . assert' 2480 . solvePart1
  input >>= solvePart2
