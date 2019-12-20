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

runComputer :: Monad m => Computer m a -> IntCode -> m (a, IntCode)
runComputer (Computer a) i = runStateT a i

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

initIntCode :: Memory -> [Int] -> IntCode
initIntCode mem is = IntCode mem 0 is []

-- | Step an intcode computer means translation of one input until Just one
-- output is produced or Nothing if it halted.
stepIntCode :: MonadFail m => IntCode -> Int -> m (Maybe Int, IntCode)
stepIntCode ic i = runComputer go (ic { inputs = i : inputs ic })
-- runIntCode :: MonadFail m => Memory -> [Int] -> m [Int]
-- runIntCode mem is = fmap outputs . runComputer go $ IntCode mem 0 is []
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
      Output a -> lookupInt a >>= return . Just
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
      Halt -> return Nothing

type Phase = Int

initAmplifier :: Memory -> Phase -> IntCode
initAmplifier mem p = initIntCode mem [p]

stepAmplifier :: IntCode -> Int -> IO (Maybe Int, IntCode)
stepAmplifier = stepIntCode

phaseSequence :: Memory -> [Int] -> IO Int
phaseSequence m [a,b,c,d,e] =
  loop (initAmplifier m a)
       (initAmplifier m b)
       (initAmplifier m c)
       (initAmplifier m d)
       (initAmplifier m e) 0
 where
  -- TODO(SN): MaybeT
  loop as bs cs ds es input =
    stepAmplifier as input >>= \case
      (Nothing, _) -> return input
      (Just ar, as') -> do
        putStrLn $ "A: " ++ show ar
        (Just br, bs') <- stepAmplifier bs ar
        putStrLn $ "B: " ++ show br
        (Just cr, cs') <- stepAmplifier cs br
        putStrLn $ "C: " ++ show cr
        (Just dr, ds') <- stepAmplifier ds cr
        putStrLn $ "D: " ++ show dr
        (Just er, es') <- stepAmplifier es dr
        putStrLn $ "E: " ++ show er
        loop as' bs' cs' ds' es' er

phaseSequence memory _ = error "invalid phase sequence"

main :: IO ()
main = do
  program <- Vector.fromList
    . map (read . Text.unpack)
    . (Text.split (== ',') . Text.decodeUtf8)
    <$> BS.readFile "day07-input.txt"
  -- let program = Vector.fromList [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]
  print =<< phaseSequence program [9,7,8,5,6]
  -- res <- fmap maximum . mapM (phaseSequence program) $ permutations [5,6,7,8,9]
  -- print res
  -- res <- mapM (\ps -> phaseSequence program ps >>= \res -> return (ps,res)) $ permutations [0,1,2,3,4]
  -- print $ maximumBy (compare `on` snd) res
