#!/usr/bin/env stack
-- stack script --resolver lts-14.4 --package text,bytestring,safe,vector,mtl
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
import           Safe

import qualified Data.ByteString        as BS
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import qualified Data.Vector            as Vector

import           Debug.Trace

type Memory = Vector Int

type Address = Int

data Instruction = Add Address Address Address
                 | Multiply Address Address Address
                 | Input Address
                 | Output Address
                 | Halt

data IntCode = IntCode { memory :: Memory
                       , pos    :: !Address
                       } deriving Show

newtype Computer m a = Computer { unComputer :: StateT IntCode m a }
                     deriving (Functor, Applicative, Monad, MonadState IntCode, MonadIO)

runComputer :: Monad m => Computer m a -> IntCode -> m a
runComputer (Computer a) i = runStateT a i >>= \(a, i) -> return a

parseInstruction :: Monad m => Computer m Instruction
parseInstruction = do
  op <- parseInt
  case op of
    1 -> do
      a <- parseInt
      b <- parseInt
      r <- parseInt
      return $ Add a b r -- TODO validate address ranges?
    2 -> do
      a <- parseInt
      b <- parseInt
      r <- parseInt
      return $ Multiply a b r -- TODO validate address ranges?
    3 -> Input <$> parseInt
    4 -> Output <$> parseInt
    99 -> return Halt
    i -> error $ "unknown opcode: " ++ show i

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

runIntCode :: Memory -> IO IntCode
runIntCode is = runComputer (go >> get) $ IntCode is 0
 where
  go = do
    op <- parseInstruction
    case op of
      Add a b r -> do
        x <- lookupInt a
        y <- lookupInt b
        modifyMemory (// [(r, x + y)])
        go
      Multiply a b r -> do
        x <- lookupInt a
        y <- lookupInt b
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
