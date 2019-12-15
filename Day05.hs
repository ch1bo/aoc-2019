#!/usr/bin/env stack
-- stack script --resolver lts-14.4 --package text,bytestring,vector,mtl
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TupleSections              #-}
module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Foldable
import           Data.Vector            (Vector, (!), (//))
import           Numeric.Natural

import qualified Data.ByteString        as BS
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import qualified Data.Vector            as Vector

import           Debug.Trace

type Memory = Vector Int

type Address = Int

data Parameter = Position Address
               | Immediate Int
               deriving Show

data Instruction = Add Parameter Parameter Address
                 | Multiply Parameter Parameter Address
                 | Input Address
                 | Output Address
                 | Halt
                 deriving Show

data IntCode = IntCode { memory :: Memory
                       , pos    :: !Address
                       } deriving Show

newtype Computer m a = Computer { unComputer :: StateT IntCode m a }
                     deriving (Functor, Applicative, Monad, MonadState IntCode, MonadIO)

runComputer :: Monad m => Computer m a -> IntCode -> m a
runComputer (Computer a) i = runStateT a i >>= \(a, i) -> return a

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
  let p1 = ins `div` 100 `mod` 10
  let p2 = ins `div` 1000 `mod` 10
  case op of
    1 -> Add <$> parseParameter p1
             <*> parseParameter p2
             <*> parseInt
    2 -> Multiply
      <$> parseParameter p1
      <*> parseParameter p2
      <*> parseInt
    3 -> Input <$> parseInt
    4 -> Output <$> parseInt
    99 -> return Halt
    i -> error $ "unknown opcode: " ++ show i

runIntCode :: Memory -> IO IntCode
runIntCode is = runComputer (go >> get) $ IntCode is 0
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
        liftIO $ putStr "Input: "
        v <- read <$> liftIO getLine
        modifyMemory (// [(a, v)])
        go
      Output a -> do
        lookupInt a >>= liftIO . putStrLn . mappend "Output: " . show
        go
      Halt -> pure ()

main :: IO ()
main = do
  diagnosticProgram <- Vector.fromList . map (read . Text.unpack) . (Text.split (== ',') . Text.decodeUtf8) <$> BS.readFile "day05-input.txt"
  print =<< runIntCode diagnosticProgram
