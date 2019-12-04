module Lib.Day4
  ( solve
  )
where

import           Lib.Util

solvePart1 :: Int
solvePart1 = length
  [ s
  | a <- ['1' .. '6']
  , b <- [a .. '9']
  , c <- [b .. '9']
  , d <- [c .. '9']
  , e <- [d .. '9']
  , f <- [e .. '9']
  , a == b || b == c || c == d || d == e || e == f
  , let s = read [a, b, c, d, e, f] :: Int
  , s >= 171309 && s <= 643603
  ]

solvePart2 :: Int
solvePart2 = length
  [ s
  | a <- ['1' .. '6']
  , b <- [a .. '9']
  , c <- [b .. '9']
  , d <- [c .. '9']
  , e <- [d .. '9']
  , f <- [e .. '9']
  , (a == b && b /= c)
    || (a /= b && b == c && c /= d)
    || (b /= c && c == d && d /= e)
    || (c /= d && d == e && e /= f)
    || (d /= e && e == f)
  , let s = read [a, b, c, d, e, f] :: Int
  , s >= 171309 && s <= 643603
  ]

solve :: IO ()
solve = do
  print $ assert' 1625 solvePart1
  print $ assert' 1111 solvePart2

