{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
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
  , isHalted
  , clearOutputs
  )
where

import           Control.Lens                   ( (%~)
                                                , (&)
                                                , makeFieldsNoPrefix
                                                , (^.)
                                                , (.~)
                                                )
import           Data.Queue                     ( Queue )
import qualified Data.Queue                    as Queue
import           Data.IntMap.Strict             ( IntMap )
import qualified Data.IntMap.Strict            as IntMap

type Memory = IntMap Int
type Noun = Int
type Verb = Int

data Input =
    InputV1 Noun Verb
  | InputV2 [Int]
  deriving (Show)

data Machine =
  Machine { _memory :: Memory
          , _instrPtr :: Int
          , _relBase :: Int
          , _inputs :: Queue Int
          , _outputs :: Queue Int
          } deriving (Show)

makeFieldsNoPrefix ''Machine

newMachine :: [Int] -> Input -> Machine
newMachine program ins =
  let (mem, inputs') = case ins of
        InputV1 noun verb ->
          let (a : _ : _ : xs) = program in (mkProgram $ a : noun : verb : xs, [])
        InputV2 ins' -> (mkProgram program, ins')
  in  Machine mem 0 0 (Queue.fromList inputs') Queue.empty
  where mkProgram p = IntMap.fromList $ zip [0 ..] p

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

decodeInstruction :: Memory -> Int -> Instruction
decodeInstruction mem index =
  let opcode          = mem IntMap.! index
      [a, b, c, _, _] = zfill 5 $ show opcode
  in  case opcode `mod` 100 of
        1 -> Add (Param (param c) (mem IntMap.! (index + 1)))
                 (Param (param b) (mem IntMap.! (index + 2)))
                 (Param (param a) (mem IntMap.! (index + 3)))
        2 -> Mul (Param (param c) (mem IntMap.! (index + 1)))
                 (Param (param b) (mem IntMap.! (index + 2)))
                 (Param (param a) (mem IntMap.! (index + 3)))
        3 -> Input (Param (param c) (mem IntMap.! (index + 1)))
        4 -> Output (Param (param c) (mem IntMap.! (index + 1)))
        5 -> JumpIfTrue (Param (param c) (mem IntMap.! (index + 1)))
                        (Param (param b) (mem IntMap.! (index + 2)))
        6 -> JumpIfFalse (Param (param c) (mem IntMap.! (index + 1)))
                         (Param (param b) (mem IntMap.! (index + 2)))
        7 -> LessThan (Param (param c) (mem IntMap.! (index + 1)))
                      (Param (param b) (mem IntMap.! (index + 2)))
                      (Param (param a) (mem IntMap.! (index + 3)))
        8 -> Equals (Param (param c) (mem IntMap.! (index + 1)))
                    (Param (param b) (mem IntMap.! (index + 2)))
                    (Param (param a) (mem IntMap.! (index + 3)))
        9  -> AdjustRelBase (Param (param c) (mem IntMap.! (index + 1)))
        99 -> Halt
        _  -> error ("decodeInstruction: " ++ show (opcode `mod` 100))
 where
  zfill :: Int -> String -> String
  zfill n cs = reverse . take n . reverse $ replicate n '0' ++ cs
  param :: Char -> ParameterMode
  param '0' = ParamPos
  param '1' = ParamImm
  param '2' = ParamRel
  param _   = error "param"

readParamValue :: Machine -> Parameter 'ParamRead -> Int
readParamValue _       (Param ParamImm n) = n
readParamValue machine (Param ParamPos n) = IntMap.findWithDefault 0 n (machine ^. memory)
readParamValue machine (Param ParamRel n) =
  IntMap.findWithDefault 0 (machine ^. relBase + n) (machine ^. memory)

writeParamValue :: Machine -> Parameter 'ParamWrite -> Int -> Machine
writeParamValue machine (Param ParamPos n) val = machine & memory %~ IntMap.insert n val
writeParamValue machine (Param ParamRel n) val =
  machine & memory %~ IntMap.insert (machine ^. relBase + n) val
writeParamValue _ (Param ParamImm _) _ = error "writeParamValue _ (Param ParamImm _) _" -- TODO: enforce with type system

runBinop
  :: (Int -> Int -> Int)
  -> Machine
  -> Parameter 'ParamRead
  -> Parameter 'ParamRead
  -> Parameter 'ParamWrite
  -> Machine
runBinop f machine param1 param2 param3 =
  let x        = readParamValue machine param1
      y        = readParamValue machine param2
      machine' = writeParamValue machine param3 (f x y)
  in  machine' & instrPtr %~ (+ 4)

runCmp
  :: (Int -> Int -> Bool)
  -> Machine
  -> Parameter 'ParamRead
  -> Parameter 'ParamRead
  -> Parameter 'ParamWrite
  -> Machine
runCmp f machine op1 op2 dest =
  let x        = readParamValue machine op1
      y        = readParamValue machine op2
      result   = if f x y then 1 else 0
      machine' = writeParamValue machine dest result
  in  machine' & instrPtr %~ (+ 4)

runJump :: (Int -> Bool) -> Machine -> Parameter 'ParamRead -> Parameter 'ParamRead -> Machine
runJump f machine test dest = if f (readParamValue machine test)
  then machine & instrPtr .~ readParamValue machine dest
  else machine & instrPtr %~ (+ 3)

runInstruction :: Machine -> Instruction -> Machine
runInstruction machine instr = case instr of
  Add src1 src2 dest -> runBinop (+) machine src1 src2 dest
  Mul src1 src2 dest -> runBinop (*) machine src1 src2 dest
  Input dest ->
    let machine' = writeParamValue machine dest (Queue.front $ machine ^. inputs)
    in  machine' & inputs %~ Queue.pop & instrPtr %~ (+ 2)
  Output src ->
    let i = readParamValue machine src in machine & outputs %~ Queue.push i & instrPtr %~ (+ 2)
  JumpIfTrue  test dest -> runJump (/= 0) machine test dest
  JumpIfFalse test dest -> runJump (== 0) machine test dest
  LessThan op1 op2 dest -> runCmp (<) machine op1 op2 dest
  Equals   op1 op2 dest -> runCmp (==) machine op1 op2 dest
  AdjustRelBase op      -> machine & relBase %~ (+ readParamValue machine op) & instrPtr %~ (+ 2)
  Halt                  -> machine

runNextInstruction :: Machine -> Machine
runNextInstruction machine =
  runInstruction machine (decodeInstruction (machine ^. memory) (machine ^. instrPtr))

isHalted :: Machine -> Bool
isHalted machine = (machine ^. memory) IntMap.! (machine ^. instrPtr) == 99

runUntil :: (Machine -> Bool) -> Machine -> Machine
runUntil p = until p runNextInstruction

runUntilHalted :: Machine -> Machine
runUntilHalted = runUntil isHalted

clearOutputs :: Machine -> Machine
clearOutputs = (& outputs .~ Queue.empty)
