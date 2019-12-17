{-# LANGUAGE DeriveTraversable #-}

module Data.Queue
  ( Queue
  , empty
  , singleton
  , fromList
  , push
  , pop
  , front
  , back
  , Data.Queue.null
  , Data.Queue.length
  )
where

import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as Seq

newtype Queue a =
  Queue { unQueue :: Seq a }
  deriving (Eq,Ord,Show,Foldable,Functor,Traversable)

empty :: Queue a
empty = Queue Seq.empty

singleton :: a -> Queue a
singleton x = fromList [x]

fromList :: [a] -> Queue a
fromList = Queue . Seq.fromList

push :: a -> Queue a -> Queue a
push x (Queue xs) = Queue $ xs Seq.|> x

pop :: Queue a -> Queue a
pop (Queue (_ Seq.:<| xs)) = Queue xs
pop _                      = error "error: empty Queue"

front :: Queue a -> a
front (Queue (x Seq.:<| _)) = x
front _                     = error "error: empty Queue"

back :: Queue a -> a
back (Queue (_ Seq.:|> x)) = x
back _                     = error "error: empty Queue"

null :: Queue a -> Bool
null = Seq.null . unQueue

length :: Queue a -> Int
length = Seq.length . unQueue
