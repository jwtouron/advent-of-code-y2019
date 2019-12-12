{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib.Day6
  ( solve
  )
where

import           Control.Lens                   ( makeFieldsNoPrefix
                                                , (&)
                                                , (.~)
                                                , (^.)
                                                )
import           Data.Foldable                  ( asum )
import           Data.List
import           Data.List.Split                ( splitOn )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromJust )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Lib.Util                       ( assert' )

stringsToMap :: [String] -> Map String (Set String)
stringsToMap =
  foldl'
      (\m [a, b] ->
        Map.insertWith Set.union a (Set.singleton b) $ Map.insertWith Set.union b Set.empty m
      )
      Map.empty
    . map (splitOn ")")

input :: IO (Map String (Set String))
input = stringsToMap . lines <$> readFile "input/day6.txt"

example1 :: Map String (Set String)
example1 =
  stringsToMap ["COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L"]

example2 :: Map String (Set String)
example2 = stringsToMap
  ["COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L", "K)YOU", "I)SAN"]

smallExample :: Map String (Set String)
smallExample = stringsToMap ["COM)B", "B)C", "B)D", "D)SAN", "C)YOU"]

data MapNode =
  Node { _name :: String
       , _orbitCount :: Int
       , _children :: [MapNode]
       } deriving (Eq,Ord,Show)

makeFieldsNoPrefix ''MapNode

buildMap :: Map String (Set String) -> MapNode
buildMap m = go (Node "COM" 0 [])
 where
  go :: MapNode -> MapNode
  go node =
    let cs = Set.toList $ m Map.! (node ^. name)
    in  node & children .~ map (\c -> go (Node c (node ^. orbitCount + 1) [])) cs

findPath :: String -> MapNode -> Maybe [MapNode]
findPath goal = go []
 where
  go :: [MapNode] -> MapNode -> Maybe [MapNode]
  go path current | (current ^. name) == goal = Just . reverse $ current : path
                  | null (current ^. children) = Nothing
                  | otherwise = asum . map (go (current : path)) $ (current ^. children)

dropShared :: Eq a => [a] -> [a] -> ([a], [a])
dropShared [] bs = ([], bs)
dropShared as [] = (as, [])
dropShared as@(a : as') bs@(b : bs') | a == b    = dropShared as' bs'
                                     | otherwise = (as, bs)

solvePart1 :: Map String (Set String) -> Int
solvePart1 m = go (buildMap m)
 where
  go :: MapNode -> Int
  go (Node _ count cs) = count + sum (map go cs)

solvePart2 :: Map String (Set String) -> Int
solvePart2 m = length as + length bs - 2
 where
  nodeMap  = buildMap m
  path1    = map (^. name) $ fromJust $ findPath "SAN" nodeMap
  path2    = map (^. name) $ fromJust $ findPath "YOU" nodeMap
  (as, bs) = dropShared path1 path2

solve :: IO ()
solve = input >>= mapM_ print . sequence [assert' 315757 . solvePart1, assert' 481 . solvePart2]
