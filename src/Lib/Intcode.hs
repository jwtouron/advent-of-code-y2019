{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Intcode
  ( Input(..)
  , Machine
  , runProgram
  , memory
  )
where

import           Control.Lens                   ( (%~)
                                                , (&)
                                                , makeFieldsNoPrefix
                                                , (^.)
                                                )
import           Control.Monad.ST               ( ST
                                                , runST
                                                )
import           Data.Vector.Unboxed            ( Vector )
import qualified Data.Vector.Unboxed           as V
import           Data.Vector.Unboxed.Mutable    ( STVector )
import qualified Data.Vector.Unboxed.Mutable   as MV

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p then' else' = p >>= \p' -> if p' then then' else else'

type STMemory s = STVector s Int
type Memory = Vector Int
type Noun = Int
type Verb = Int

data Input = Input { noun :: Noun, verb :: Verb } deriving (Show)

data Machine = Machine { _memory :: Memory } deriving (Show)

makeFieldsNoPrefix ''Machine

data STMachine s = STMachine { _memory :: STMemory s, _instrPtr :: Int }

makeFieldsNoPrefix ''STMachine

newSTMachine :: [Int] -> ST s (STMachine s)
newSTMachine = (return . flip STMachine 0 =<<) . V.thaw . V.fromList

freezeSTMachine :: STMachine s -> ST s Machine
freezeSTMachine machine = Machine <$> V.unsafeFreeze (machine ^. memory)

data Instruction =
    Add Int Int Int
  | Mul Int Int Int
  | Halt
  deriving (Show)

decodeInstruction :: STMemory s -> Int -> ST s Instruction
decodeInstruction memory index = do
  opcode <- MV.unsafeRead memory index
  case opcode of
    1 ->
      Add
        <$> MV.unsafeRead memory (index + 1)
        <*> MV.unsafeRead memory (index + 2)
        <*> MV.unsafeRead memory (index + 3)
    2 ->
      Mul
        <$> MV.unsafeRead memory (index + 1)
        <*> MV.unsafeRead memory (index + 2)
        <*> MV.unsafeRead memory (index + 3)
    99 -> return Halt
    _  -> error "decodeInstruction"

runBinop :: (Int -> Int -> Int) -> STMemory s -> Int -> Int -> Int -> ST s ()
runBinop f memory src1 src2 dest = do
  x <- MV.unsafeRead memory src1
  y <- MV.unsafeRead memory src2
  MV.unsafeWrite memory dest (f x y)

runInstruction :: STMemory s -> Instruction -> ST s ()
runInstruction memory (Add src1 src2 dest) = runBinop (+) memory src1 src2 dest
runInstruction memory (Mul src1 src2 dest) = runBinop (*) memory src1 src2 dest
runInstruction _      Halt                 = return ()

advanceInstrPtr :: Instruction -> Int
advanceInstrPtr Add{}  = 4
advanceInstrPtr Mul{}  = 4
advanceInstrPtr Halt{} = 1

runNextInstruction :: STMachine s -> ST s (STMachine s)
runNextInstruction machine = do
  instr <- decodeInstruction (machine ^. memory) (machine ^. instrPtr)
  runInstruction (machine ^. memory) instr
  return $ machine & instrPtr %~ (+ advanceInstrPtr instr)

isHalted :: STMachine s -> ST s Bool
isHalted machine = (== 99) <$> MV.unsafeRead (machine ^. memory) (machine ^. instrPtr)

runProgram :: [Int] -> Input -> Machine
runProgram program (Input noun verb) = runST $ do
  machine <- newSTMachine program
  MV.unsafeWrite (machine ^. memory) 1 noun
  MV.unsafeWrite (machine ^. memory) 2 verb
  loop machine >>= freezeSTMachine
 where
  loop :: STMachine s -> ST s (STMachine s)
  loop machine = ifM (isHalted machine) (return machine) (runNextInstruction machine >>= loop)
