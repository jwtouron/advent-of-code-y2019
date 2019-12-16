{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Lib.Intcode
  ( Input(..)
  , Machine
  , memory
  , instrPtr
  , inputs
  , outputs
  , runUntil
  , runUntilHalted
  , newMachine
  , isHalted
  )
where

import           Control.Lens                   ( (%~)
                                                , (&)
                                                , makeFieldsNoPrefix
                                                , (^.)
                                                , (.~)
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

data Input =
    InputV1 Noun Verb
  | InputV2 [Int]
  deriving (Show)

data IMachine a =
  IMachine { _memory :: a
           , _instrPtr :: Int
           , _inputs :: [Int]
           , _outputs :: [Int]
           } deriving (Show)

makeFieldsNoPrefix ''IMachine

type STMachine s = IMachine (STMemory s)
type Machine = IMachine Memory

newMachine :: [Int] -> Input -> Machine
newMachine program ins =
  let (mem, inputs') = case ins of
        InputV1 noun verb ->
          let (a : _ : _ : xs) = program in (V.fromList (a : noun : verb : xs), [])
        InputV2 ins' -> (V.fromList program, ins')
  in  IMachine mem 0 inputs' []

type family ThawFreezeMemory s a where
  ThawFreezeMemory s (STVector s Int) = Vector Int
  ThawFreezeMemory s (Vector Int) = STVector s Int

thawFreezeMachine
  :: (a -> ST s (ThawFreezeMemory s a)) -> IMachine a -> ST s (IMachine (ThawFreezeMemory s a))
thawFreezeMachine f machine =
  IMachine
    <$> f (machine ^. memory)
    <*> pure (machine ^. instrPtr)
    <*> pure (machine ^. inputs)
    <*> pure (machine ^. outputs)

thawMachine :: Machine -> ST s (STMachine s)
thawMachine = thawFreezeMachine V.thaw

unsafeFreezeSTMachine :: STMachine s -> ST s Machine
unsafeFreezeSTMachine = thawFreezeMachine V.unsafeFreeze

data Parameter = Imm Int | Pos Int | Write Int deriving (Show)

data Instruction =
    Add Parameter Parameter Parameter
  | Mul Parameter Parameter Parameter
  | Input Parameter
  | Output Parameter
  | JumpIfTrue Parameter Parameter
  | JumpIfFalse Parameter Parameter
  | LessThan Parameter Parameter Parameter
  | Equals Parameter Parameter Parameter
  | Halt
  deriving (Show)

decodeInstruction :: STMemory s -> Int -> ST s Instruction
decodeInstruction mem index = do
  opcode <- MV.unsafeRead mem index
  let [_, b, c, _, _] = zfill 5 $ show opcode
  case opcode `mod` 100 of
    1 ->
      Add
        <$> (param c <$> MV.unsafeRead mem (index + 1))
        <*> (param b <$> MV.unsafeRead mem (index + 2))
        <*> (Write <$> MV.unsafeRead mem (index + 3))
    2 ->
      Mul
        <$> (param c <$> MV.unsafeRead mem (index + 1))
        <*> (param b <$> MV.unsafeRead mem (index + 2))
        <*> (Write <$> MV.unsafeRead mem (index + 3))
    3 -> Input <$> (Write <$> MV.unsafeRead mem (index + 1))
    4 -> Output <$> (param c <$> MV.unsafeRead mem (index + 1))
    5 ->
      JumpIfTrue
        <$> (param c <$> MV.unsafeRead mem (index + 1))
        <*> (param b <$> MV.unsafeRead mem (index + 2))
    6 ->
      JumpIfFalse
        <$> (param c <$> MV.unsafeRead mem (index + 1))
        <*> (param b <$> MV.unsafeRead mem (index + 2))
    7 ->
      LessThan
        <$> (param c <$> MV.unsafeRead mem (index + 1))
        <*> (param b <$> MV.unsafeRead mem (index + 2))
        <*> (Write <$> MV.unsafeRead mem (index + 3))
    8 ->
      Equals
        <$> (param c <$> MV.unsafeRead mem (index + 1))
        <*> (param b <$> MV.unsafeRead mem (index + 2))
        <*> (Write <$> MV.unsafeRead mem (index + 3))
    99 -> return Halt
    _  -> error ("decodeInstruction: " ++ show (opcode `mod` 100))
 where
  zfill :: Int -> String -> String
  zfill n cs = reverse . take n . reverse $ replicate n '0' ++ cs
  param :: Char -> Int -> Parameter
  param '0' = Pos
  param '1' = Imm
  param _   = error "param"

paramValue :: STMemory s -> Parameter -> ST s Int
paramValue _   (Imm   n) = return n
paramValue mem (Pos   n) = MV.unsafeRead mem n
paramValue _   (Write n) = return n

runBinop
  :: forall s
   . (Int -> Int -> Int)
  -> STMachine s
  -> Parameter
  -> Parameter
  -> Parameter
  -> ST s (STMachine s)
runBinop f machine param1 param2 param3 = do
  x    <- paramValue (machine ^. memory) param1
  y    <- paramValue (machine ^. memory) param2
  dest <- paramValue (machine ^. memory) param3
  MV.unsafeWrite (machine ^. memory) dest (f x y)
  return $ machine & instrPtr %~ (+ 4)

runCmp
  :: (Int -> Int -> Bool)
  -> STMachine s
  -> Parameter
  -> Parameter
  -> Parameter
  -> ST s (STMachine s)
runCmp f machine op1 op2 dest = do
  result <-
    (\x y -> if f x y then 1 else 0)
    <$> paramValue (machine ^. memory) op1
    <*> paramValue (machine ^. memory) op2
  dest' <- paramValue (machine ^. memory) dest
  MV.unsafeWrite (machine ^. memory) dest' result
  return $ machine & instrPtr %~ (+ 4)

runJump :: (Int -> Bool) -> STMachine s -> Parameter -> Parameter -> ST s (STMachine s)
runJump f machine test dest = do
  true <- f <$> paramValue (machine ^. memory) test
  if true
    then paramValue (machine ^. memory) dest >>= (\d -> return $ machine & instrPtr .~ d)
    else return $ machine & instrPtr %~ (+ 3)

runInstruction :: STMachine s -> Instruction -> ST s (STMachine s)
runInstruction machine instr = case instr of
  Add src1 src2 dest -> runBinop (+) machine src1 src2 dest
  Mul src1 src2 dest -> runBinop (*) machine src1 src2 dest
  Input dest         -> do
    paramValue (machine ^. memory) dest
      >>= (\d -> MV.unsafeWrite (machine ^. memory) d (head (machine ^. inputs)))
    return $ machine & inputs %~ tail & instrPtr %~ (+ 2)
  Output src -> do
    i <- paramValue (machine ^. memory) src
    return $ machine & outputs %~ (i :) & instrPtr %~ (+ 2)
  JumpIfTrue  test dest -> runJump (/= 0) machine test dest
  JumpIfFalse test dest -> runJump (== 0) machine test dest
  LessThan op1 op2 dest -> runCmp (<) machine op1 op2 dest
  Equals   op1 op2 dest -> runCmp (==) machine op1 op2 dest
  Halt                  -> return machine

runNextInstruction :: STMachine s -> ST s (STMachine s)
runNextInstruction machine =
  decodeInstruction (machine ^. memory) (machine ^. instrPtr) >>= runInstruction machine

isHalted :: Machine -> Bool
isHalted machine = (machine ^. memory) V.! (machine ^. instrPtr) == 99

runUntil :: (Machine -> Bool) -> Machine -> Machine
runUntil p machine = runST $ thawMachine machine >>= loop >>= unsafeFreezeSTMachine
 where
  loop :: STMachine s -> ST s (STMachine s)
  loop m = ifM (p <$> unsafeFreezeSTMachine m) (return m) (runNextInstruction m >>= loop)

runUntilHalted :: Machine -> Machine
runUntilHalted = runUntil isHalted
