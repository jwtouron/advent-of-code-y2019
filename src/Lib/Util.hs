module Lib.Util
  ( assert'
  ) where

import           Control.Exception (assert)

assert' :: Eq a => a -> a -> a
assert' expected actual = assert (expected == actual) actual
