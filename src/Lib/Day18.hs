-- Answer to part 1 is 4954

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Lib.Day18
  ( spec
  , solvePart1
  , solvePart2
  , solve
  , input
  )
where

import           Algorithm.Search               ( dijkstraM )
import           Control.Exception              ( assert )
import           Control.Lens                  as Lens
                                                ( (&)
                                                , (%~)
                                                , (^.)
                                                , (.~)
                                                , to
                                                , makeFieldsNoPrefix
                                                )
import           Control.Monad.State.Lazy
import           Data.Bits
import           Data.Char                      ( isLower
                                                , isUpper
                                                , chr
                                                , ord
                                                )
import           Data.List                      ( foldl' )
import qualified Data.Map.Lazy                 as LMap
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromJust )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Word

import           Debug.Trace
traceShow' a = traceShow a a

input :: IO String
input = readFile "input/day18.txt"

example1 :: String
example1 = "#########\n\
           \#b.A.@.a#\n\
           \#########"

example2 :: String
example2 =
  "########################\n\
  \#f.D.E.e.C.b.A.@.a.B.c.#\n\
  \######################.#\n\
  \#d.....................#\n\
  \########################"

example3 :: String
example3 =
  "########################\n\
  \#...............b.C.D.f#\n\
  \#.######################\n\
  \#.....@.a.B.c.d.A.e.F.g#\n\
  \########################"

example4 :: String
example4 =
  "#################\n\
  \#i.G..c...e..H.p#\n\
  \########.########\n\
  \#j.A..b...f..D.o#\n\
  \########@########\n\
  \#k.E..a...g..B.n#\n\
  \########.########\n\
  \#l.F..d...h..C.m#\n\
  \#################"

example5 :: String
example5 =
  "########################\n\
  \#@..............ac.GI.b#\n\
  \###d#e#f################\n\
  \###A#B#C################\n\
  \###g#h#i################\n\
  \########################"

example6 :: String -- 8 steps
example6 =
  "#######\n\
  \#a.#Cd#\n\
  \##...##\n\
  \##.@.##\n\
  \##...##\n\
  \#cB#Ab#\n\
  \#######"

example7 :: String -- 24 steps
example7 =
  "###############\n\
  \#d.ABC.#.....a#\n\
  \######...######\n\
  \######.@.######\n\
  \######...######\n\
  \#b.....#.....c#\n\
  \###############"

example8 :: String -- 32 steps
example8 =
  "#############\n\
  \#DcBa.#.GhKl#\n\
  \#.###...#I###\n\
  \#e#d#.@.#j#k#\n\
  \###C#...###J#\n\
  \#fEbA.#.FgHi#\n\
  \#############"

type LMap = LMap.Map

data CharCase = Upper | Lower deriving (Show)

newtype CharSet (a :: CharCase) = CharSet { unCharSet :: Word32 } deriving (Eq,Ord,Show,Num,Bits)

data CasedChar (a :: CharCase) =
    LowerCC { unCasedChar :: !Char }
  | UpperCC { unCasedChar :: !Char }
  deriving (Eq,Ord,Show)

class HasOffset (a ::CharCase) where
  offset :: Int

instance HasOffset 'Upper where
  offset = 65

instance HasOffset 'Lower where
  offset = 97

mkCasedCharLower :: Char -> CasedChar 'Lower
mkCasedCharLower c = assert (isLower c) $ LowerCC c

mkCasedCharUpper :: Char -> CasedChar 'Upper
mkCasedCharUpper c = assert (isUpper c) $ UpperCC c

casedCharOffset :: forall  a . HasOffset a => CasedChar a -> Int
casedCharOffset (LowerCC c) = ord c - offset @a
casedCharOffset (UpperCC c) = ord c - offset @a

charSetInsert :: HasOffset a => CasedChar a -> CharSet a -> CharSet a
charSetInsert cc cs = setBit cs (casedCharOffset cc)

mkCasedChar :: Char -> CasedChar a
mkCasedChar c | isLower c = LowerCC c
              | isUpper c = UpperCC c
              | otherwise = error "mkCasedChar"

charSetToList :: forall  a . HasOffset a => CharSet a -> [CasedChar a]
charSetToList cs = foldr
  (\i cs' -> if testBit cs i then mkCasedChar (chr (offset @a + i)) : cs' else cs')
  []
  [0 .. 25]

charSetIsSubset :: CharSet a -> CharSet b -> Bool
charSetIsSubset cs1 cs2 = unCharSet cs1 .&. unCharSet cs2 == unCharSet cs1

type Location = (Int, Int)
type Doors = CharSet 'Upper
type Keys = CharSet 'Lower
type Key = CasedChar 'Lower

data Vault =
  Vault { _locChars :: Map Location Char
        , _keys :: Keys
        , _keyLocs :: Map Key Location
        , _doors :: Doors
        , _startLocs :: Set Location
        } deriving (Show)

makeFieldsNoPrefix ''Vault

parseVaultDesc :: String -> Vault
parseVaultDesc vaultDesc = vault
 where
  vault = foldl' f (Vault Map.empty 0 Map.empty 0 Set.empty) . zip [0 ..] . lines $ vaultDesc
  f s (y, l) = foldl' (g y) s $ zip [0 ..] l
  g y v (x, c) =
    let v' = case c of
          _ | isLower c ->
            let cc = mkCasedCharLower c
            in  v & (keys %~ charSetInsert cc) & (keyLocs %~ Map.insert cc (x, y))
          _ | isUpper c -> v & doors %~ charSetInsert (mkCasedCharUpper c)
          '@'           -> v & startLocs %~ Set.insert (x, y)
          _             -> v
    in  v' & locChars %~ Map.insert (x, y) c

convertVaultToP2 :: Vault -> Vault
convertVaultToP2 vault = vault' & startLocs .~ Set.fromList (map fst newStartLocs)
 where
  vault'   = foldl' (\v (l, c) -> v & locChars %~ Map.insert l c) vault $ newWalls ++ newStartLocs
  (x, y)   = vault ^. startLocs . to Set.toList . to head
  newWalls = [ (loc, '#') | loc <- [(x, y), (x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)] ]
  newStartLocs =
    [ (loc, '@') | loc <- [(x + 1, y + 1), (x - 1, y - 1), (x + 1, y - 1), (x - 1, y + 1)] ]

data SmallSearchState =
  SmallSearchState { _location :: !Location
                   , _doors :: !Doors
                   , _distance :: !Int
                   } deriving (Eq,Ord,Show)

makeFieldsNoPrefix ''SmallSearchState

findAllKeyPaths :: Vault -> Location -> LMap Location [(Doors, Int)]
findAllKeyPaths vault loc =
  LMap.fromListWith (++)
    $ map (\ss -> (ss ^. location, [(ss ^. doors, ss ^. distance)]))
    $ Set.toList
    $ go (Set.singleton loc) (Set.singleton startState) Set.empty
 where
  go :: Set Location -> Set SmallSearchState -> Set SmallSearchState -> Set SmallSearchState
  go visited frontier result
    | Set.null frontier
    = result
    | otherwise
    = let frontierNeighbors              = concatMap neighbors $ Set.toList frontier
          (visited', frontier', result') = foldl' g (visited, Set.empty, result) frontierNeighbors
      in  go visited' frontier' result'
   where
    g (visited, frontier, result) ss =
      let visited' = ss ^. location . to (`Set.insert` visited)
          (frontier', result') =
              let c = searchStateChar ss
              in  if isLower c || c == '@'
                    then (frontier, Set.insert ss result)
                    else (Set.insert ss frontier, result)
      in  (visited', frontier', result')
    neighbors ss =
      [ ss
          & (location .~ n)
          & (doors %~ \ds -> if isUpper c then charSetInsert (mkCasedCharUpper c) ds else ds)
          & (distance %~ (+ 1))
      | let loc = ss ^. location
      , let (x, y) = loc
      , n <- [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
      , not $ Set.member n visited
      , let c = searchStateChar ss
      , c /= '#'
      ]
  startState = SmallSearchState loc 0 0
  searchStateChar ss = vault ^. locChars . to (Map.! (ss ^. location))

data LargeSearchStateLocation =
  LSSLocation { _start :: !Location
              , _current :: !Location
              } deriving (Eq,Ord,Show)

makeFieldsNoPrefix ''LargeSearchStateLocation

data LargeSearchState =
  LargeSearchState { _locations :: Set LargeSearchStateLocation
                   , _keysCollected :: !Keys
                   , _distance :: !Int
                   } deriving (Show)

makeFieldsNoPrefix ''LargeSearchState

instance Eq LargeSearchState where
  s1 == s2 = s1 ^. locations == s2 ^. locations && s1 ^. keysCollected == s2 ^. keysCollected

instance Ord LargeSearchState where
  s1 `compare` s2 =
    ((s1 ^. locations) `compare` (s2 ^. locations))
      <> ((s1 ^. keysCollected) `compare` (s2 ^. keysCollected))

type LargeSearchCache = LMap Location (LMap Location [(Doors, Int)])

findAllKeys :: Vault -> (Int, [LargeSearchState])
findAllKeys vault = fromJust $ flip evalState Map.empty $ dijkstraM neighbors
                                                                    cost
                                                                    atGoal
                                                                    startState
 where
  neighbors :: LargeSearchState -> State LargeSearchCache [LargeSearchState]
  neighbors st = do
    modify updateCache
    cache <- get
    return $ concatMap (f cache) $ Set.toList $ st ^. locations
   where
    f :: LargeSearchCache -> LargeSearchStateLocation -> [LargeSearchState]
    f cache stateLoc = do
      let loc = stateLoc ^. current
      loc' <- stateLoc ^. start : vault ^. keyLocs . to Map.elems
      guard $ loc /= loc'
      let md = minDistance cache loc loc' (st ^. keysCollected)
      guard $ md /= inf
      return
        $ st
        & (locations %~ Set.insert (stateLoc & current .~ loc') . Set.delete stateLoc)
        & (keysCollected %~ \kc -> if Set.member loc' (vault ^. startLocs)
            then kc
            else charSetInsert (mkCasedCharLower (vault ^. locChars . to (Map.! loc'))) kc
          )
        & (distance %~ (+ round md))
    minDistance :: LargeSearchCache -> Location -> Location -> CharSet 'Lower -> Double
    minDistance cache l1 l2 ks = maybe inf (maybe inf f . LMap.lookup l2) $ LMap.lookup l1 cache
     where
      f = foldl'
        (\minDist (drs, dist) -> if charSetIsSubset drs ks && fromIntegral dist < minDist
          then fromIntegral dist
          else minDist
        )
        inf
    updateCache cache =
      foldl' (\c l -> if LMap.member l c then c else LMap.insert l (findAllKeyPaths vault l) c)
             cache
        $ map (^. current)
        $ Set.toList (st ^. locations)
  cost :: LargeSearchState -> LargeSearchState -> State LargeSearchCache Int
  cost s1 s2 = return $ s2 ^. distance - s1 ^. distance
  atGoal :: LargeSearchState -> State LargeSearchCache Bool
  atGoal s = return $ s ^. keysCollected == vault ^. keys
  startState =
    LargeSearchState (Set.fromList [ LSSLocation l l | l <- Set.toList (vault ^. startLocs) ]) 0 0
  inf = (1 / 0) :: Double

solvePart1 :: String -> Int
solvePart1 input = assert (result == 4954) result
  where result = fst . findAllKeys . parseVaultDesc $ input

solvePart2 :: String -> Int
solvePart2 input = assert (result == 2334) result
  where result = fst . findAllKeys . convertVaultToP2 . parseVaultDesc $ input

solve :: IO ()
solve = do
  input >>= print . solvePart1
  input >>= print . solvePart2

spec = undefined
