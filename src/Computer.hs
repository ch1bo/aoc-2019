{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TupleSections              #-}
module Computer where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Foldable
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Vector            (Vector, (!), (!?), (//))
import           Debug.Trace

import qualified Data.ByteString        as BS
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import qualified Data.Vector            as Vector

type Memory = Vector Integer

type Address = Int

data Parameter = Position Address
               | Immediate Integer
               | Relative Address
               deriving Show

data Instruction = Add Parameter Parameter Parameter
                 | Multiply Parameter Parameter Parameter
                 | Input Parameter
                 | Output Parameter
                 | JumpIfTrue Parameter Parameter
                 | JumpIfFalse Parameter Parameter
                 | LessThan Parameter Parameter Parameter
                 | Equals Parameter Parameter Parameter
                 | RelativeBaseOffset Parameter
                 | Halt
                 deriving Show

data IntCode = IntCode { memory :: !Memory
                       , pos    :: !Address
                       , base   :: !Address
                       } deriving Show

mkIntCode :: Memory -> IntCode
mkIntCode p = IntCode p 0 0

type Computer m = (Monad m, MonadState IntCode m)

parseInt :: Computer m => m Integer
parseInt = do
  i <- get
  res <- lookupInt (pos i)
  put (i { pos = pos i + 1})
  return res

lookupInt :: Computer m => Address -> m Integer
lookupInt a = do
  ensureMemory (a+1)
  gets (\i -> memory i !? a) >>= \case
    Nothing -> error "aa"
    Just i -> pure i

writeMemory :: Computer m => Address -> Integer -> m ()
writeMemory a v = ensureMemory (a+1) >> modify (\i -> i { memory = (memory i // [(a, v)]) })

ensureMemory :: Computer m => Int -> m ()
ensureMemory s = modify $ \i -> i { memory = grow (memory i) }
 where
  grow m | length m < s = m <> Vector.replicate (s - length m) 0
         | otherwise = m

jump :: Computer m => Address -> m ()
jump a = modify (\i -> i { pos = a })

offsetBase :: Computer m => Address -> m ()
offsetBase a = modify (\i -> i { base = (base i) + a })

parseAddress :: Computer m => m Address
parseAddress = fromInteger <$> parseInt

parseParameter :: Computer m => Integer -> m Parameter
parseParameter 0 = Position . fromInteger <$> parseInt
parseParameter 1 = Immediate <$> parseInt
parseParameter 2 = Relative . fromInteger <$> parseInt
parseParameter m = error $ "invalid parameter mode: " ++ show m

lookupParameter :: Computer m => Parameter -> m Integer
lookupParameter (Position a)  = lookupInt a
lookupParameter (Immediate v) = pure v
lookupParameter (Relative a)  = get >>= \ic -> lookupInt (base ic + a)

writeParameter :: Computer m => Parameter -> Integer -> m ()
writeParameter (Position a) v = writeMemory a v
writeParameter (Relative a) v = get >>= \ic -> writeMemory (base ic + a) v
writeParameter _ _ = error "only Position and Relative can be written to"

parseInstruction :: Computer m => m Instruction
parseInstruction = do
  ins <- parseInt
  let op = ins `mod` 100
  let pm1 = ins `div` 100 `mod` 10
  let pm2 = ins `div` 1000 `mod` 10
  let pm3 = ins `div` 10000 `mod` 10
  case op of
    1 -> Add <$> parseParameter pm1
             <*> parseParameter pm2
             <*> parseParameter pm3
    2 -> Multiply
      <$> parseParameter pm1
      <*> parseParameter pm2
      <*> parseParameter pm3
    3 -> Input <$> parseParameter pm1
    4 -> Output <$> parseParameter pm1
    5 -> JumpIfTrue <$> parseParameter pm1 <*> parseParameter pm2
    6 -> JumpIfFalse <$> parseParameter pm1 <*> parseParameter pm2
    7 -> LessThan <$> parseParameter pm1 <*> parseParameter pm2 <*> parseParameter pm3
    8 -> Equals <$> parseParameter pm1 <*> parseParameter pm2 <*> parseParameter pm3
    9 -> RelativeBaseOffset <$> parseParameter pm1
    99 -> return Halt
    i -> error $ "unknown opcode: " ++ show i

-- | Run until next 'Output' and provide given 'Integer' on every 'Input'.
stepComputer :: Computer m => (m Integer) -> m (Maybe Integer)
stepComputer input = go
 where
  go = do
    op <- parseInstruction
    -- case trace (show op) op of
    case op of
      Add a b r -> do
        x <- lookupParameter a
        y <- lookupParameter b
        writeParameter r (x + y)
        go
      Multiply a b r -> do
        x <- lookupParameter a
        y <- lookupParameter b
        writeParameter r (x * y)
        go
      Input a -> do
        v <- input
        writeParameter a v
        go
      Output a -> do
        v <- lookupParameter a
        return $ Just v
      JumpIfTrue a to -> do
        x <- lookupParameter a
        when (x /= 0) $ lookupParameter to >>= jump . fromInteger
        go
      JumpIfFalse a to -> do
        x <- lookupParameter a
        when (x == 0) $ lookupParameter to >>= jump . fromInteger
        go
      LessThan a b r -> do
        x <- lookupParameter a
        y <- lookupParameter b
        writeParameter r (if x < y then 1 else 0)
        go
      Equals a b r -> do
        x <- lookupParameter a
        y <- lookupParameter b
        writeParameter r (if x == y then 1 else 0)
        go
      RelativeBaseOffset a -> do
        x <- lookupParameter a
        offsetBase (fromInteger x)
        go
      Halt -> return Nothing

class Monad m => ComputerIO m where
  input :: m Integer
  output :: Integer -> m ()

instance ComputerIO IO where
  input = readLn
  output = print

-- | Run until 'Halt'.
runComputer :: ComputerIO m => IntCode -> m IntCode
runComputer = execStateT go
 where
  go = stepComputer (lift input) >>= \case
    Nothing -> return ()
    Just o -> lift (output o) >> go

loadIntCode :: FilePath -> IO IntCode
loadIntCode fp =
  mkIntCode
  <$> Vector.fromList
  . map (read . Text.unpack)
  . (Text.split (== ',') . Text.decodeUtf8)
  <$> BS.readFile fp
