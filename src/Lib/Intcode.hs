{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Intcode
  ( Input(..)
  , Machine
  , memory
  , runProgram
  , outputs
  )
where

import           Control.Lens                   ( (%~)
                                                , (&)
                                                , makeFieldsNoPrefix
                                                , (^.)
                                                , (.~)
                                                )
import           Control.Monad                  ( when )
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
    InputV1 { noun :: Noun, verb :: Verb }
  | InputV2 { unInputV2 :: [Int] }
  deriving (Show)

isInputV1 :: Input -> Bool
isInputV1 InputV1{} = True
isInputV1 _         = False

inputToList :: Input -> [Int]
inputToList input = case input of
  InputV1{}      -> []
  InputV2 inputs -> inputs

data Machine = Machine { _memory :: Memory, _outputs :: [Int] } deriving (Show)

makeFieldsNoPrefix ''Machine

data STMachine s = STMachine { _memory :: STMemory s, _instrPtr :: Int, _inputs :: [Int], _outputs :: [Int] }

makeFieldsNoPrefix ''STMachine

newSTMachine :: [Int] -> [Int] -> ST s (STMachine s)
newSTMachine program ins =
  V.thaw (V.fromList program) >>= (\mem -> return $ STMachine mem 0 ins [])

freezeSTMachine :: STMachine s -> ST s Machine
freezeSTMachine machine =
  Machine <$> V.unsafeFreeze (machine ^. memory) <*> pure (machine ^. outputs)

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

encodeParam :: STMemory s -> Parameter -> ST s Int
encodeParam _   (Imm   n) = return n
encodeParam mem (Pos   n) = MV.unsafeRead mem n
encodeParam _   (Write n) = return n

runBinop
  :: forall s
   . (Int -> Int -> Int)
  -> STMachine s
  -> Parameter
  -> Parameter
  -> Parameter
  -> ST s (STMachine s)
runBinop f machine param1 param2 param3 = do
  x    <- encodeParam (machine ^. memory) param1
  y    <- encodeParam (machine ^. memory) param2
  dest <- encodeParam (machine ^. memory) param3
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
    <$> encodeParam (machine ^. memory) op1
    <*> encodeParam (machine ^. memory) op2
  dest' <- encodeParam (machine ^. memory) dest
  MV.unsafeWrite (machine ^. memory) dest' result
  return $ machine & instrPtr %~ (+ 4)

runJump :: (Int -> Bool) -> STMachine s -> Parameter -> Parameter -> ST s (STMachine s)
runJump f machine test dest = do
  true <- f <$> encodeParam (machine ^. memory) test
  if true
    then encodeParam (machine ^. memory) dest >>= (\d -> return $ machine & instrPtr .~ d)
    else return $ machine & instrPtr %~ (+ 3)

runInstruction :: STMachine s -> Instruction -> ST s (STMachine s)
runInstruction machine instr = case instr of
  Add src1 src2 dest -> runBinop (+) machine src1 src2 dest
  Mul src1 src2 dest -> runBinop (*) machine src1 src2 dest
  Input dest         -> do
    encodeParam (machine ^. memory) dest
      >>= (\d -> MV.unsafeWrite (machine ^. memory) d (head (machine ^. inputs)))
    return $ machine & inputs %~ tail & instrPtr %~ (+ 2)
  Output src -> do
    i <- encodeParam (machine ^. memory) src
    return $ machine & outputs %~ (i :) & instrPtr %~ (+ 2)
  JumpIfTrue  test dest -> runJump (/= 0) machine test dest
  JumpIfFalse test dest -> runJump (== 0) machine test dest
  LessThan op1 op2 dest -> runCmp (<) machine op1 op2 dest
  Equals   op1 op2 dest -> runCmp (==) machine op1 op2 dest
  Halt                  -> return machine

runNextInstruction :: STMachine s -> ST s (STMachine s)
runNextInstruction machine =
  decodeInstruction (machine ^. memory) (machine ^. instrPtr) >>= runInstruction machine

isHalted :: STMachine s -> ST s Bool
isHalted machine = (== 99) <$> MV.unsafeRead (machine ^. memory) (machine ^. instrPtr)

runProgram :: [Int] -> Input -> Machine
runProgram program input = runST $ do
  machine <- newSTMachine program (inputToList input)
  when (isInputV1 input) $ do
    MV.unsafeWrite (machine ^. memory) 1 (noun input)
    MV.unsafeWrite (machine ^. memory) 2 (verb input)
  loop machine >>= freezeSTMachine
 where
  loop :: STMachine s -> ST s (STMachine s)
  loop machine = ifM (isHalted machine) (return machine) (runNextInstruction machine >>= loop)
