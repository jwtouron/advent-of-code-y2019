module Lib.Day14
  ( spec
  )
where

import           Data.List                      ( foldl' )
import           Data.List.Split                ( splitOn )
import qualified Data.Map.Strict               as Map
import           Lib.Util                hiding ( example )
import           Data.Graph                    as Graph

data Quantity = Quant Int String deriving (Show)
data Reaction = React [Quantity] Quantity deriving (Show)

parseQuantity :: String -> Quantity
parseQuantity s = let [x, y] = words s in Quant (read x) y

parseReaction :: String -> Reaction
parseReaction s =
  let [q1, q2] = splitOn " => " s in React (map parseQuantity $ splitOn ", " q1) (parseQuantity q2)

example1 :: [Reaction]
example1 =
  map parseReaction
    $ lines
        "9 ORE => 2 A\n\
        \8 ORE => 3 B\n\
        \7 ORE => 5 C\n\
        \3 A, 4 B => 1 AB\n\
        \5 B, 7 C => 1 BC\n\
        \4 C, 1 A => 1 CA\n\
        \2 AB, 3 BC, 4 CA => 1 FUEL"

example2 :: [Reaction]
example2 =
  map parseReaction
    $ lines
        "157 ORE => 5 NZVS\n\
        \165 ORE => 6 DCFZ\n\
        \44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL\n\
        \12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ\n\
        \179 ORE => 7 PSHF\n\
        \177 ORE => 5 HKGWZ\n\
        \7 DCFZ, 7 PSHF => 2 XJWVT\n\
        \165 ORE => 2 GPVTF\n\
        \3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"

input :: IO [Reaction]
input = map parseReaction . lines <$> readFile "input/day14.txt"

findMinimumOre :: Quantity -> [Reaction] -> Int
findMinimumOre (Quant n chem) reactions =
  flip (Map.!) "ORE"
    $ foldl' f (updateNeededMap Map.empty chem n)
    $ map (\v -> let (_, k, _) = nodeFromVertex v in k)
    $ tail
    $ topSort graph
 where
  updateNeededMap neededMap chem needed =
    let (React quants (Quant n _)) = reactionMap Map.! chem
        batches                    = (needed + n - 1) `div` n
    in  foldl' (\m (Quant n c) -> Map.insertWith (+) c (batches * n) m) neededMap quants
  f m chem = updateNeededMap m chem (m Map.! chem)
  (graph, nodeFromVertex, _) = graphFromEdges $ foldr f [] reactions
    where f (React qs (Quant _ s)) rs = (s, s, map (\(Quant _ s) -> s) qs) : rs
  reactionMap = foldl' (\m r@(React _ (Quant _ s)) -> Map.insert s r m) Map.empty reactions

solvePart1 :: [Reaction] -> Int
solvePart1 = findMinimumOre (Quant 1 "FUEL")

solvePart2 :: [Reaction] -> Int
solvePart2 reactions = go 0
 where
  go :: Int -> Int
  go base =
    let base' = expSearch base
    in  if findMinimumOre (Quant (base' + 1) "FUEL") reactions > limit then base' else go base'
  expSearch base = foldr
    (\n res ->
      let m = findMinimumOre (Quant (base + (n + 1) ^ (2 :: Int)) "FUEL") reactions
      in  if m > limit then base + n ^ (2 :: Int) else res
    )
    (-1)
    [1 ..]
  limit = 1000 * 1000 * 1000 * 1000

spec :: Spec
spec = mkSpec input 14 [(`shouldBe` 741927) . solvePart1, (`shouldBe` 2371699) . solvePart2]
