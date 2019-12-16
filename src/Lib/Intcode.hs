{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

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
  , newMachineWithSize
  , isHalted
  )
where

import           Control.Lens                   ( (%~)
                                                , (&)
                                                , makeFieldsNoPrefix
                                                , (^.)
                                                , (.~)
                                                )
import           Control.Monad                  ( forM_ )
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
           , _relBase :: Int
           , _inputs :: [Int]
           , _outputs :: [Int]
           }

makeFieldsNoPrefix ''IMachine

instance Show (IMachine (Vector Int)) where
  show machine =
    "IMachine {_memory = "
      ++ show (reverse $ dropWhile (== 0) $ reverse $ V.toList $ machine ^. memory)
      ++ ", _instrPtr = "
      ++ show (machine ^. instrPtr)
      ++ ", _relbase = "
      ++ show (machine ^. relBase)
      ++ ", _inputs = "
      ++ show (machine ^. inputs)
      ++ ", _outputs = "
      ++ show (machine ^. outputs)
      ++ "}"

type STMachine s = IMachine (STMemory s)
type Machine = IMachine Memory

newMachineWithSize :: Int -> [Int] -> Input -> Machine
newMachineWithSize memorySize program ins =
  let (mem, inputs') = case ins of
        InputV1 noun verb ->
          let (a : _ : _ : xs) = program in (mkProgram $ a : noun : verb : xs, [])
        InputV2 ins' -> (mkProgram program, ins')
  in  IMachine mem 0 0 inputs' []
 where
  mkProgram p = if memorySize <= 0
    then V.fromList p
    else runST $ do
      vec <- MV.new memorySize
      forM_ (zip [0 ..] p) (uncurry (MV.unsafeWrite vec))
      V.unsafeFreeze vec

newMachine :: [Int] -> Input -> Machine
newMachine = newMachineWithSize 0

type family ThawFreezeMemory s a where
  ThawFreezeMemory s (STVector s Int) = Vector Int
  ThawFreezeMemory s (Vector Int) = STVector s Int

thawFreezeMachine
  :: (a -> ST s (ThawFreezeMemory s a)) -> IMachine a -> ST s (IMachine (ThawFreezeMemory s a))
thawFreezeMachine f machine =
  IMachine
    <$> f (machine ^. memory)
    <*> pure (machine ^. instrPtr)
    <*> pure (machine ^. relBase)
    <*> pure (machine ^. inputs)
    <*> pure (machine ^. outputs)

thawMachine :: Machine -> ST s (STMachine s)
thawMachine = thawFreezeMachine V.thaw

unsafeFreezeSTMachine :: STMachine s -> ST s Machine
unsafeFreezeSTMachine = thawFreezeMachine V.unsafeFreeze

data ParameterRW = ParamRead | ParamWrite deriving (Show)
data ParameterMode = ParamPos | ParamImm | ParamRel deriving (Show)
data Parameter (a :: ParameterRW) = Param ParameterMode Int deriving (Show)

data Instruction where
  Add ::Parameter 'ParamRead -> Parameter 'ParamRead -> Parameter 'ParamWrite -> Instruction
  Mul ::Parameter 'ParamRead -> Parameter 'ParamRead -> Parameter 'ParamWrite -> Instruction
  Input ::Parameter 'ParamWrite -> Instruction
  Output ::Parameter 'ParamRead -> Instruction
  JumpIfTrue ::Parameter 'ParamRead -> Parameter 'ParamRead -> Instruction
  JumpIfFalse ::Parameter 'ParamRead -> Parameter 'ParamRead -> Instruction
  LessThan ::Parameter 'ParamRead -> Parameter 'ParamRead -> Parameter 'ParamWrite -> Instruction
  Equals ::Parameter 'ParamRead -> Parameter 'ParamRead -> Parameter 'ParamWrite -> Instruction
  AdjustRelBase ::Parameter 'ParamRead -> Instruction
  Halt ::Instruction

deriving instance (Show Instruction)

decodeInstruction :: STMemory s -> Int -> ST s Instruction
decodeInstruction mem index = do
  opcode <- MV.unsafeRead mem index
  let [a, b, c, _, _] = zfill 5 $ show opcode
  case opcode `mod` 100 of
    1 ->
      Add
        <$> (Param (param c) <$> MV.unsafeRead mem (index + 1))
        <*> (Param (param b) <$> MV.unsafeRead mem (index + 2))
        <*> (Param (param a) <$> MV.unsafeRead mem (index + 3))
    2 ->
      Mul
        <$> (Param (param c) <$> MV.unsafeRead mem (index + 1))
        <*> (Param (param b) <$> MV.unsafeRead mem (index + 2))
        <*> (Param (param a) <$> MV.unsafeRead mem (index + 3))
    3 -> Input <$> (Param (param c) <$> MV.unsafeRead mem (index + 1))
    4 -> Output <$> (Param (param c) <$> MV.unsafeRead mem (index + 1))
    5 ->
      JumpIfTrue
        <$> (Param (param c) <$> MV.unsafeRead mem (index + 1))
        <*> (Param (param b) <$> MV.unsafeRead mem (index + 2))
    6 ->
      JumpIfFalse
        <$> (Param (param c) <$> MV.unsafeRead mem (index + 1))
        <*> (Param (param b) <$> MV.unsafeRead mem (index + 2))
    7 ->
      LessThan
        <$> (Param (param c) <$> MV.unsafeRead mem (index + 1))
        <*> (Param (param b) <$> MV.unsafeRead mem (index + 2))
        <*> (Param (param a) <$> MV.unsafeRead mem (index + 3))
    8 ->
      Equals
        <$> (Param (param c) <$> MV.unsafeRead mem (index + 1))
        <*> (Param (param b) <$> MV.unsafeRead mem (index + 2))
        <*> (Param (param a) <$> MV.unsafeRead mem (index + 3))
    9  -> AdjustRelBase <$> (Param (param c) <$> MV.unsafeRead mem (index + 1))
    99 -> return Halt
    _  -> error ("decodeInstruction: " ++ show (opcode `mod` 100))
 where
  zfill :: Int -> String -> String
  zfill n cs = reverse . take n . reverse $ replicate n '0' ++ cs
  param :: Char -> ParameterMode
  param '0' = ParamPos
  param '1' = ParamImm
  param '2' = ParamRel
  param _   = error "param"

readParamValue :: STMachine s -> Parameter 'ParamRead -> ST s Int
readParamValue _       (Param ParamImm n) = return n
readParamValue machine (Param ParamPos n) = MV.unsafeRead (machine ^. memory) n
readParamValue machine (Param ParamRel n) =
  MV.unsafeRead (machine ^. memory) (machine ^. relBase + n)

writeParamValue :: STMachine s -> Parameter 'ParamWrite -> Int -> ST s ()
writeParamValue machine (Param ParamPos n) val = MV.unsafeWrite (machine ^. memory) n val
writeParamValue machine (Param ParamRel n) val =
  MV.unsafeWrite (machine ^. memory) (machine ^. relBase + n) val
writeParamValue _ (Param ParamImm _) _ = error "writeParamValue _ (Param ParamImm _) _" -- TODO: enforce with type system

runBinop
  :: forall s
   . (Int -> Int -> Int)
  -> STMachine s
  -> Parameter 'ParamRead
  -> Parameter 'ParamRead
  -> Parameter 'ParamWrite
  -> ST s (STMachine s)
runBinop f machine param1 param2 param3 = do
  x <- readParamValue machine param1
  y <- readParamValue machine param2
  writeParamValue machine param3 (f x y)
  return $ machine & instrPtr %~ (+ 4)

runCmp
  :: (Int -> Int -> Bool)
  -> STMachine s
  -> Parameter 'ParamRead
  -> Parameter 'ParamRead
  -> Parameter 'ParamWrite
  -> ST s (STMachine s)
runCmp f machine op1 op2 dest = do
  result <-
    (\x y -> if f x y then 1 else 0) <$> readParamValue machine op1 <*> readParamValue machine op2
  writeParamValue machine dest result
  return $ machine & instrPtr %~ (+ 4)

runJump
  :: (Int -> Bool)
  -> STMachine s
  -> Parameter 'ParamRead
  -> Parameter 'ParamRead
  -> ST s (STMachine s)
runJump f machine test dest = do
  true <- f <$> readParamValue machine test
  if true
    then readParamValue machine dest >>= (\d -> return $ machine & instrPtr .~ d)
    else return $ machine & instrPtr %~ (+ 3)

runInstruction :: STMachine s -> Instruction -> ST s (STMachine s)
runInstruction machine instr = case instr of
  Add src1 src2 dest -> runBinop (+) machine src1 src2 dest
  Mul src1 src2 dest -> runBinop (*) machine src1 src2 dest
  Input dest         -> do
    writeParamValue machine dest (head (machine ^. inputs))
    return $ machine & inputs %~ tail & instrPtr %~ (+ 2)
  Output src -> do
    i <- readParamValue machine src
    return $ machine & outputs %~ (i :) & instrPtr %~ (+ 2)
  JumpIfTrue  test dest -> runJump (/= 0) machine test dest
  JumpIfFalse test dest -> runJump (== 0) machine test dest
  LessThan op1 op2 dest -> runCmp (<) machine op1 op2 dest
  Equals   op1 op2 dest -> runCmp (==) machine op1 op2 dest
  AdjustRelBase op ->
    readParamValue machine op
      >>= (\op' -> return $ machine & relBase %~ (+ op') & instrPtr %~ (+ 2))
  Halt -> return machine

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
