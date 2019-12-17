{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib.Day12
  ( spec
  )
where

import           Control.Applicative            ( (<|>) )
import           Control.Lens
import           Control.Monad
import           Data.IntMap.Strict             ( IntMap )
import qualified Data.IntMap.Strict            as IntMap
import           Data.List                      ( foldl' )
import           Data.List.Split                ( splitOn )
import           Lib.Util                hiding ( example )

data Position = Pos { _x :: Int, _y :: Int, _z :: Int } deriving (Eq,Ord,Show)
data Velocity = Vel { _x :: Int, _y :: Int, _z :: Int } deriving (Eq,Ord,Show)
data Moon = Moon { _moonId :: Int, _position :: Position, _velocity :: Velocity } deriving (Eq,Ord,Show)

makeFieldsNoPrefix ''Position
makeFieldsNoPrefix ''Velocity
makeFieldsNoPrefix ''Moon

newtype Moons = Moons { moons :: IntMap Moon } deriving (Eq,Ord,Show)

newMoonFromList :: Int -> [Int] -> Moon
newMoonFromList id [x, y, z] = Moon id (Pos x y z) (Vel 0 0 0)
newMoonFromList _  _         = error "newMoonFromList"

input :: IO Moons
input =
  Moons
    .   IntMap.fromList
    .   map
          (\(i, l) ->
            (i, newMoonFromList i $ map (read . last . splitOn "=") . splitOn ", " . init . tail $ l)
          )
    .   zip [1 ..]
    .   lines
    <$> readFile "input/day12.txt"

example :: Moons
example = Moons $ IntMap.fromList $ zipWith (\i xs -> (i, newMoonFromList i xs))
                                            [1 ..]
                                            [[-1, 0, 2], [2, -10, -7], [4, -8, 8], [3, 5, -1]]

updateVelocities :: (Moon, Moon) -> (Moon, Moon)
updateVelocities (moon1, moon2) = (moon1', moon2')
 where
  velX = case (moon1 ^. position . x, moon2 ^. position . x) of
    (x1, x2) | x1 < x2 -> 1
    (x1, x2) | x1 > x2 -> -1
    _                  -> 0
  velY = case (moon1 ^. position . y, moon2 ^. position . y) of
    (y1, y2) | y1 < y2 -> 1
    (y1, y2) | y1 > y2 -> -1
    _                  -> 0
  velZ = case (moon1 ^. position . z, moon2 ^. position . z) of
    (z1, z2) | z1 < z2 -> 1
    (z1, z2) | z1 > z2 -> -1
    _                  -> 0
  moon1' = moon1 & velocity . x %~ (+ velX) & velocity . y %~ (+ velY) & velocity . z %~ (+ velZ)
  moon2' = moon2 & velocity . x %~ sub velX & velocity . y %~ sub velY & velocity . z %~ sub velZ
  sub    = subtract

updateAllVelocities :: Moons -> Moons
updateAllVelocities ms = go (filter (\[x, y] -> x < y) $ replicateM 2 $ IntMap.keys $ moons ms) ms
 where
  go :: [[Int]] -> Moons -> Moons
  go [] moons = moons
  go ([id1, id2] : pairs) (Moons moons) =
    let (m1', m2') = updateVelocities (moons IntMap.! id1, moons IntMap.! id2)
        moons'     = IntMap.insert id1 m1' $ IntMap.insert id2 m2' moons
    in  go pairs (Moons moons')
  go _ _ = error "go"

updatePosition :: Moon -> Moon
updatePosition moon =
  moon & position . x %~ (+ vel x) & position . y %~ (+ vel y) & position . z %~ (+ vel z)
  where vel p = moon ^. velocity . p

step :: Moons -> Moons
step =
  Moons . foldl' f IntMap.empty . map updatePosition . IntMap.elems . moons . updateAllVelocities
  where f moonMap moon = IntMap.insert (moon ^. moonId) moon moonMap

stepN :: Int -> Moons -> Moons
stepN n = head . drop n . iterate step

moonEnergy :: Moon -> Int
moonEnergy moon = potential * kinetic
 where
  potential = abs (moon ^. position . x) + abs (moon ^. position . y) + abs (moon ^. position . z)
  kinetic   = abs (moon ^. velocity . x) + abs (moon ^. velocity . y) + abs (moon ^. velocity . z)

moonsEnergy :: Moons -> Int
moonsEnergy = sum . map moonEnergy . IntMap.elems . moons

solvePart1 :: Moons -> Int
solvePart1 = moonsEnergy . stepN 1000

solvePart2 :: Moons -> Int
solvePart2 ms =
  let (Just x1, Just x2, Just x3) = go 0 (Nothing, Nothing, Nothing) ms in lcm x1 $ lcm x2 x3
 where
  go :: Int -> (Maybe Int, Maybe Int, Maybe Int) -> Moons -> (Maybe Int, Maybe Int, Maybe Int)
  go _ seen@(Just _, Just _, Just _) _ = seen
  go n seen ms =
    let ms'    = step ms :: Moons
        counts = getCounts ms' :: ([Int], [Int], [Int])
        f idx = (<|> if counts ^. idx == initial ^. idx then Just (n + 1) else Nothing)
        seen' = seen & _1 %~ f _1 & _2 %~ f _2 & _3 %~ f _3
    in  go (n + 1) seen' ms'
  initial   = getCounts ms
  getCounts = foldl' f ([], [], []) . IntMap.elems . moons
  f (xs, ys, zs) moon =
    ( moon ^. position . x : moon ^. velocity . x : xs
    , moon ^. position . y : moon ^. velocity . y : ys
    , moon ^. position . z : moon ^. velocity . z : zs
    )

spec :: Spec
spec = mkSpec input 12 [(`shouldBe` 7687) . solvePart1, (`shouldBe` 334945516288044) . solvePart2]
