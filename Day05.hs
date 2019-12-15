#!/usr/bin/env stack
-- stack script --resolver lts-14.4 --package text,bytestring,safe,vector,mtl
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TupleSections              #-}
module Main where

import           Control.Monad
import           Control.Monad.State
import           Data.Foldable
import           Data.Vector         (Vector, (!), (//))
import           Numeric.Natural
import           Safe

import qualified Data.ByteString     as BS
import qualified Data.Text           as Text
import qualified Data.Text.Encoding  as Text
import qualified Data.Vector         as Vector

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

newtype Computer a = Computer { unComputer :: State IntCode a }
                   deriving (Functor, Applicative, Monad, MonadState IntCode)

runComputer :: Computer a -> IntCode -> a
runComputer (Computer a) i = fst $ runState a i

parseInstruction :: Computer Instruction
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

parseInt :: Computer Int
parseInt = do
  i <- get
  res <- lookupInt (pos i)
  put (i { pos = pos i + 1})
  return res

lookupInt :: Address -> Computer Int
lookupInt a = gets (\i -> memory i ! a)

modifyMemory :: (Memory -> Memory) -> Computer ()
modifyMemory f = modify (\i -> i { memory = f $ memory i })

runIntCode :: Memory -> IntCode
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
      Input a -> go
      Output a -> go
      Halt -> pure ()

main :: IO ()
main = do
  diagnosticProgram <- Vector.fromList . map (read . Text.unpack) . (Text.split (== ',') . Text.decodeUtf8) <$> BS.readFile "day05-input.txt"
  print $ runIntCode diagnosticProgram
