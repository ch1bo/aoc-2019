#!/usr/bin/env stack
-- stack script --resolver lts-14.4 --package text,bytestring,vector,mtl,transformers
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TupleSections              #-}
module Main where

import           Control.Monad
import           Control.Monad.Fail
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Foldable
import           Data.Function
import           Data.List
import           Data.Vector            (Vector, (!), (//))
import           Numeric.Natural

import qualified Data.ByteString        as BS
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import qualified Data.Vector            as Vector

type Memory = Vector Int

type Address = Int

data Parameter = Position Address
               | Immediate Int
               deriving Show

data Instruction = Add Parameter Parameter Address
                 | Multiply Parameter Parameter Address
                 | Input Address
                 | Output Address
                 | JumpIfTrue Parameter Parameter
                 | JumpIfFalse Parameter Parameter
                 | LessThan Parameter Parameter Address
                 | Equals Parameter Parameter Address
                 | Halt
                 deriving Show

data IntCode = IntCode { memory  :: Memory
                       , pos     :: !Address
                       , inputs  :: [Int]
                       , outputs :: [Int]
                       } deriving Show

newtype Computer m a = Computer { unComputer :: StateT IntCode m a }
                     deriving (Functor, Applicative, Monad, MonadState IntCode, MonadIO, MonadFail)

runComputer :: Monad m => Computer m a -> IntCode -> m IntCode
runComputer (Computer a) i = runStateT a i >>= \(a, i) -> return i

parseInt :: Monad m => Computer m Int
parseInt = do
  i <- get
  res <- lookupInt (pos i)
  put (i { pos = pos i + 1})
  return res

lookupInt :: Monad m => Address -> Computer m Int
lookupInt a = gets (\i -> memory i ! a)

modifyMemory :: Monad m => (Memory -> Memory) -> Computer m ()
modifyMemory f = modify (\i -> i { memory = f $ memory i })

jump :: Monad m => Address -> Computer m ()
jump a = modify (\i -> i { pos = a })

parseParameter :: Monad m => Int -> Computer m Parameter
parseParameter 0 = Position <$> parseInt
parseParameter 1 = Immediate <$> parseInt
parseParameter m = error $ "invalid parameter mode: " ++ show m

parameterValue :: Monad m => Parameter -> Computer m Int
parameterValue (Position a)  = lookupInt a
parameterValue (Immediate v) = pure v

parseInstruction :: Monad m => Computer m Instruction
parseInstruction = do
  ins <- parseInt
  let op = ins `mod` 100
  let pm1 = ins `div` 100 `mod` 10
  let pm2 = ins `div` 1000 `mod` 10
  case op of
    1 -> Add <$> parseParameter pm1
             <*> parseParameter pm2
             <*> parseInt
    2 -> Multiply
      <$> parseParameter pm1
      <*> parseParameter pm2
      <*> parseInt
    3 -> Input <$> parseInt
    4 -> Output <$> parseInt
    5 -> JumpIfTrue <$> parseParameter pm1 <*> parseParameter pm2
    6 -> JumpIfFalse <$> parseParameter pm1 <*> parseParameter pm2
    7 -> LessThan <$> parseParameter pm1 <*> parseParameter pm2 <*> parseInt
    8 -> Equals <$> parseParameter pm1 <*> parseParameter pm2 <*> parseInt
    99 -> return Halt
    i -> error $ "unknown opcode: " ++ show i

popInput :: MonadFail m => Computer m Int
popInput =
  gets inputs >>= \case
    [] -> error "no input left"
    (i:is) -> do
      modify (\ic -> ic { inputs = is })
      return i

pushOutput :: Monad m => Int -> Computer m ()
pushOutput o = modify (\ic -> ic { outputs = o : outputs ic })

runIntCode :: MonadFail m => Memory -> [Int] -> m [Int]
runIntCode mem is = fmap outputs . runComputer go $ IntCode mem 0 is []
 where
  go = do
    op <- parseInstruction
    case op of
      Add a b r -> do
        x <- parameterValue a
        y <- parameterValue b
        modifyMemory (// [(r, x + y)])
        go
      Multiply a b r -> do
        x <- parameterValue a
        y <- parameterValue b
        modifyMemory (// [(r, x * y)])
        go
      Input a -> do
        v <- popInput
        modifyMemory (// [(a, v)])
        go
      Output a -> do
        lookupInt a >>= pushOutput
        go
      JumpIfTrue a to -> do
        x <- parameterValue a
        when (x /= 0) $ parameterValue to >>= jump
        go
      JumpIfFalse a to -> do
        x <- parameterValue a
        when (x == 0) $ parameterValue to >>= jump
        go
      LessThan a b r -> do
        x <- parameterValue a
        y <- parameterValue b
        modifyMemory (// [(r, if x < y then 1 else 0)])
        go
      Equals a b r -> do
        x <- parameterValue a
        y <- parameterValue b
        modifyMemory (// [(r, if x == y then 1 else 0)])
        go
      Halt -> pure ()

type Phase = Int

amplifier :: Memory -> Phase -> [Int] -> IO [Int]
amplifier mem p is = runIntCode mem (p:is)

phaseSequence :: Memory -> [Int] -> IO Int
phaseSequence m [a,b,c,d,e] = fmap head $
  amplifier m a [0] >>= amplifier m b >>= amplifier m c >>= amplifier m d >>= amplifier m e
phaseSequence memory _ = error "invalid phase sequence"

main :: IO ()
main = do
  program <- Vector.fromList
    . map (read . Text.unpack)
    . (Text.split (== ',') . Text.decodeUtf8)
    <$> BS.readFile "day07-input.txt"

  -- print =<< amplifier program 0 [0]
  -- print =<< phaseSequence program [3,1,2,4,0]
  res <- fmap maximum . mapM (phaseSequence program) $ permutations [0,1,2,3,4]
  print res
  -- res <- mapM (\ps -> phaseSequence program ps >>= \res -> return (ps,res)) $ permutations [0,1,2,3,4]
  -- print $ maximumBy (compare `on` snd) res
