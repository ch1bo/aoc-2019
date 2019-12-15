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

main :: IO ()
main = do
  program <- Vector.fromList
    . map (read . Text.unpack)
    . (Text.split (== ',') . Text.decodeUtf8)
    <$> BS.readFile "day05-input.txt"
  -- let program = Vector.fromList [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
  void $ runIntCode program
