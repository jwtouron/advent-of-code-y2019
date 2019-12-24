{-# LANGUAGE BangPatterns #-}

module Lib.Day16
  ( spec
  )
where

import           Data.Char                      ( digitToInt
                                                , intToDigit
                                                )
import           Data.Foldable                  ( toList )
import           Data.List                      ( foldl' )
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as Seq
import           Lib.Util

input :: IO [Int]
input = map digitToInt . init <$> readFile "input/day16.txt"

stepPart1 :: [Int] -> [Int]
stepPart1 xs = foldr (f xs) [] $ zip [1 ..] xs
 where
  f xs (idx, _) xs' =
    let pattern = tail $ cycle $ replicate idx 0 ++ replicate idx 1 ++ replicate idx 0 ++ replicate
          idx
          (-1)
    in  ((abs $ sum $ zipWith (*) xs pattern) `mod` 10) : xs'

solvePart1 :: [Int] -> String
solvePart1 xs = map intToDigit $ take 8 $ head $ drop 100 $ iterate stepPart1 xs

stepPart2 :: Seq Int -> Seq Int
stepPart2 xs = fst $ foldl' f (Seq.empty, 0) [Seq.length xs - 1, Seq.length xs - 2 .. 0]
 where
  f (!seq', !sum') idx =
    let sum'' = Seq.index xs idx + sum'
        seq'' = sum'' `mod` 10 Seq.<| seq'
    in  (seq'', sum'')

solvePart2 :: [Int] -> String
solvePart2 xs = map intToDigit $ take 8 $ toList final
 where
  final = head $ drop 100 $ iterate stepPart2 $ Seq.fromList $ drop msgOffset $ concat
    (replicate 10000 xs)
  msgOffset = read $ map intToDigit $ take 7 xs

spec :: Spec
spec = mkSpec input 16 [(`shouldBe` "30550349") . solvePart1, (`shouldBe` "62938399") . solvePart2]
