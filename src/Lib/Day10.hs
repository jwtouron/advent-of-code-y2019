module Lib.Day10
  ( spec
  )
where

import           Control.Monad                  ( guard )
import           Data.List                      ( foldl'
                                                , groupBy
                                                , maximumBy
                                                , sortBy
                                                )
import qualified Data.Map                      as Map
import           Data.Ord                       ( comparing )
import           Data.Ratio
import qualified Data.Set                      as Set
import           Lib.Util                       ( manhattenDistance
                                                , Spec
                                                , mkSpec
                                                , shouldBe
                                                )

parseAsteroids :: String -> [(Int, Int)]
parseAsteroids = foldl' f [] . zip [0 ..] . lines
  where f s (r, l) = foldl' (\s' (c, x) -> if x == '#' then (c, r) : s' else s') s $ zip [0 ..] l

input :: IO [(Int, Int)]
input = parseAsteroids <$> readFile "input/day10.txt"

example0 :: [(Int, Int)]
example0 =
  parseAsteroids ".#..#\n\
         \.....\n\
         \#####\n\
         \....#\n\
         \...##"

example1 :: [(Int, Int)]
example1 =
  parseAsteroids
    "......#.#.\n\
    \#..#.#....\n\
    \..#######.\n\
    \.#.#.###..\n\
    \.#..#.....\n\
    \..#....#.#\n\
    \#..#....#.\n\
    \.##.#..###\n\
    \##...#..#.\n\
    \.#....####"

example2 :: [(Int, Int)]
example2 =
  parseAsteroids
    ".#..##.###...#######\n\
    \##.############..##.\n\
    \.#.######.########.#\n\
    \.###.#######.####.#.\n\
    \#####.##.#.##.###.##\n\
    \..#####..#.#########\n\
    \####################\n\
    \#.####....###.#.#.##\n\
    \##.#################\n\
    \#####.##.###..####..\n\
    \..######..##.#######\n\
    \####.##.####...##..#\n\
    \.#####..#.######.###\n\
    \##...#.##########...\n\
    \#.##########.#######\n\
    \.####.#.###.###.#.##\n\
    \....##.##.###..#####\n\
    \.#.#.###########.###\n\
    \#.#.#.#####.####.###\n\
    \###.##.####.##.#..##"

data Relationship =
  Rel { asteroid1 :: (Int, Int)
      , slope :: Ratio Int
      , distance :: Int
      , asteroid2 :: (Int, Int)
      } deriving (Show)

relationshipAtan2 :: Relationship -> Double
relationshipAtan2 Rel { asteroid1 = (c1, r1), asteroid2 = (c2, r2) } =
  atan2 (fromIntegral (c2 - c1)) (fromIntegral (r2 - r1))

verticalSlope :: Ratio Int
verticalSlope = 999999999

relationshipsForAsteroid :: (Int, Int) -> [(Int, Int)] -> [Relationship]
relationshipsForAsteroid a1@(c1, r1) asteroids = do
  a2@(c2, r2) <- asteroids
  guard $ a1 /= a2
  let slope' = if c1 == c2 then verticalSlope else abs (r1 - r2) % abs (c1 - c2)
  return $ Rel a1 slope' (manhattenDistance a1 a2) a2

bestViewingAsteroid :: [(Int, Int)] -> ((Int, Int), Int)
bestViewingAsteroid asteroids = maximumBy (comparing snd) $ Map.assocs slopeMap
 where
  slopeMap = foldl'
    (\m a -> Map.insert
      a
      (Set.size $ Set.fromList $ map relationshipAtan2 $ relationshipsForAsteroid a asteroids)
      m
    )
    Map.empty
    asteroids

solvePart1 :: [(Int, Int)] -> Int
solvePart1 = snd . bestViewingAsteroid

solvePart2 :: [(Int, Int)] -> Int
solvePart2 asteroids = x * 100 + y
 where
  Rel { asteroid2 = (x, y) } = go 199 $ groupAsteroids $ sortBy sortBy' rels
  go :: Int -> [[Relationship]] -> Relationship
  go 0 ((asteroid : _) : _        ) = asteroid
  go n ([_           ] : asteroids) = go (n - 1) asteroids
  go n ((_ : as1     ) : as2      ) = go (n - 1) (as2 ++ [as1])
  go _ _                            = error "go"
  groupAsteroids :: [Relationship] -> [[Relationship]]
  groupAsteroids = groupBy (\r1 r2 -> relationshipAtan2 r1 == relationshipAtan2 r2)
  sortBy' r1 r2 = case (relationshipAtan2 r1, relationshipAtan2 r2) of
    (x, y) | x > y -> LT
    (x, y) | x < y -> GT
    _              -> compare (distance r1) (distance r2)
  bestAsteroid = bestViewingAsteroid asteroids
  rels         = relationshipsForAsteroid (fst bestAsteroid) asteroids

spec :: Spec
spec = mkSpec input 10 [flip shouldBe 230 . solvePart1, flip shouldBe 1205 . solvePart2]
