#!/usr/bin/env stack
-- stack script --resolver lts-14.4 --package text,bytestring,vector,mtl,async
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TupleSections              #-}
module Main where

import           Debug.Trace

import           Control.Concurrent.Async
import           Control.Concurrent.Chan
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Foldable
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Vector              (Vector, (!), (//))
import           Numeric.Natural

import qualified Data.ByteString          as BS
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import qualified Data.Vector              as Vector

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

runAmplifier :: Memory -> String -> Chan Int -> Chan Int -> IO (Maybe Int)
runAmplifier mem name ic oc =
  runComputer (go ic oc Nothing) $ IntCode mem 0
 where
  go ic oc res = trace (name ++ " res: " ++ show res) $ do
    op <- parseInstruction
    case op of
      Add a b r -> do
        x <- parameterValue a
        y <- parameterValue b
        modifyMemory (// [(r, x + y)])
        go ic oc res
      Multiply a b r -> do
        x <- parameterValue a
        y <- parameterValue b
        modifyMemory (// [(r, x * y)])
        go ic oc res
      Input a -> do
        v <- liftIO $ readChan ic
        trace (name ++ " input: " ++ show v) $ modifyMemory (// [(a, v)])
        go ic oc res
      Output a -> do
        v <- lookupInt a
        trace (name ++ " output: " ++ show v) $ liftIO $ writeChan oc v
        go ic oc (Just v)
      JumpIfTrue a to -> do
        x <- parameterValue a
        when (x /= 0) $ parameterValue to >>= jump
        go ic oc res
      JumpIfFalse a to -> do
        x <- parameterValue a
        when (x == 0) $ parameterValue to >>= jump
        go ic oc res
      LessThan a b r -> do
        x <- parameterValue a
        y <- parameterValue b
        modifyMemory (// [(r, if x < y then 1 else 0)])
        go ic oc res
      Equals a b r -> do
        x <- parameterValue a
        y <- parameterValue b
        modifyMemory (// [(r, if x == y then 1 else 0)])
        go ic oc res
      Halt -> trace (name ++ " halted") $ pure res

phaseSequence :: Memory -> [Int] -> IO Int
phaseSequence m [a,b,c,d,e] = do
  ain <- newChan
  bin <- newChan
  cin <- newChan
  din <- newChan
  ein <- newChan
  _ <- writeChan ain a >> async (runAmplifier m "A" ain bin)
  _ <- writeChan bin b >> async (runAmplifier m "B" bin cin)
  _ <- writeChan cin c >> async (runAmplifier m "C" cin din)
  _ <- writeChan din d >> async (runAmplifier m "D" din ein)
  ampe <- writeChan ein e >> async (runAmplifier m "E" ein ain)
  writeChan ain 0
  wait ampe >>= \case
    Nothing -> error "amplifier E never produced output"
    Just res -> return res

phaseSequence memory _ = error "invalid phase sequence"

main :: IO ()
main = do
  program <- Vector.fromList
    . map (read . Text.unpack)
    . (Text.split (== ',') . Text.decodeUtf8)
    <$> BS.readFile "day07-input.txt"
  -- let program = Vector.fromList [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]
  -- let program = Vector.fromList [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
  -- print =<< phaseSequence program [9,7,8,5,6]
  -- res <- fmap maximum . mapM (phaseSequence program) $ permutations [5,6,7,8,9]
  -- print res
  res <- mapM (\ps -> phaseSequence program ps >>= \res -> return (ps,res)) $ permutations [5,6,7,8,9]
  print $ maximumBy (compare `on` snd) res
