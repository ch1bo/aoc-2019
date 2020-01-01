#!/usr/bin/env stack
-- stack script --resolver lts-14.4 --package text,bytestring,vector,mtl,async,containers
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TupleSections              #-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.Chan
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Foldable
import           Data.Function
import           Data.List
import           Data.Map                 (Map)
import           Data.Maybe
import           Data.Vector              (Vector, (!?), (//))
import           Debug.Trace
import           Text.Printf

import qualified Data.ByteString          as BS
import qualified Data.Map                 as Map
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import qualified Data.Vector              as Vector

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

newtype Computer m a = Computer { unComputer :: StateT IntCode m a }
                     deriving (Functor, Applicative, Monad, MonadState IntCode, MonadIO)

runComputer :: Monad m => Computer m a -> IntCode -> m a
runComputer (Computer a) i = runStateT a i >>= \(a, i) -> return a

parseInt :: Monad m => Computer m Integer
parseInt = do
  i <- get
  res <- lookupInt (pos i)
  put (i { pos = pos i + 1})
  return res

lookupInt :: Monad m => Address -> Computer m Integer
lookupInt a = do
  ensureMemory (a+1)
  gets (\i -> memory i !? a) >>= \case
    Nothing -> error "aa"
    Just i -> pure i

writeMemory :: Monad m => Address -> Integer -> Computer m ()
writeMemory a v = ensureMemory (a+1) >> modify (\i -> i { memory = (memory i // [(a, v)]) })

ensureMemory :: Monad m => Int -> Computer m ()
ensureMemory s = modify $ \i -> i { memory = grow (memory i) }
 where
  grow m | length m < s = m <> Vector.replicate (s - length m) 0
         | otherwise = m

jump :: Monad m => Address -> Computer m ()
jump a = modify (\i -> i { pos = a })

offsetBase :: Monad m => Address -> Computer m ()
offsetBase a = modify (\i -> i { base = (base i) + a })

parseAddress :: Monad m => Computer m Address
parseAddress = fromInteger <$> parseInt

parseParameter :: Monad m => Integer -> Computer m Parameter
parseParameter 0 = Position . fromInteger <$> parseInt
parseParameter 1 = Immediate <$> parseInt
parseParameter 2 = Relative . fromInteger <$> parseInt
parseParameter m = error $ "invalid parameter mode: " ++ show m

lookupParameter :: Monad m => Parameter -> Computer m Integer
lookupParameter (Position a)  = lookupInt a
lookupParameter (Immediate v) = pure v
lookupParameter (Relative a)  = get >>= \ic -> lookupInt (base ic + a)

writeParameter :: Monad m => Parameter -> Integer -> Computer m ()
writeParameter (Position a) v = writeMemory a v
writeParameter (Relative a) v = get >>= \ic -> writeMemory (base ic + a) v
writeParameter _ _ = error "only Position and Relative can be written to"

parseInstruction :: Monad m => Computer m Instruction
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

runIntCodeIO :: Memory -> IO ()
runIntCodeIO mem = do
  ic <- newChan
  oc <- newChan
  race_ (runIntCodeChan mem ic oc >> threadDelay 10000) $
    race_ (forever $ readChan oc >>= putStrLn . show) $
      forever $ readLn >>= writeChan ic

runIntCodeChan :: Memory -> Chan Integer -> Chan Integer -> IO (Maybe Integer)
runIntCodeChan mem ic oc =
  runComputer (drawRow ic oc Nothing) $ IntCode mem 0 0
 where
  drawRow ic oc res = do
    op <- parseInstruction
    -- case trace (show op) op of
    case op of
      Add a b r -> do
        x <- lookupParameter a
        y <- lookupParameter b
        writeParameter r (x + y)
        drawRow ic oc res
      Multiply a b r -> do
        x <- lookupParameter a
        y <- lookupParameter b
        writeParameter r (x * y)
        drawRow ic oc res
      Input a -> do
        v <- liftIO $ readChan ic
        writeParameter a v
        drawRow ic oc res
      Output a -> do
        v <- lookupParameter a
        liftIO $ writeChan oc v
        drawRow ic oc (Just v)
      JumpIfTrue a to -> do
        x <- lookupParameter a
        when (x /= 0) $ lookupParameter to >>= jump . fromInteger
        drawRow ic oc res
      JumpIfFalse a to -> do
        x <- lookupParameter a
        when (x == 0) $ lookupParameter to >>= jump . fromInteger
        drawRow ic oc res
      LessThan a b r -> do
        x <- lookupParameter a
        y <- lookupParameter b
        writeParameter r (if x < y then 1 else 0)
        drawRow ic oc res
      Equals a b r -> do
        x <- lookupParameter a
        y <- lookupParameter b
        writeParameter r (if x == y then 1 else 0)
        drawRow ic oc res
      RelativeBaseOffset a -> do
        x <- lookupParameter a
        offsetBase (fromInteger x)
        drawRow ic oc res
      Halt -> pure res

-- New code

type Pos = (Int,Int)

data Dir = U | R | D | L deriving Show

type Path = Map Pos Int

lookupColor :: Path -> Pos -> Int
lookupColor m k = maybe 0 id $ Map.lookup k m

paintColor :: Pos -> Int -> Path -> Path
paintColor = Map.insert

runRobot :: Memory -> IO Path
runRobot m = do
  ic <- newChan
  oc <- newChan
  comp <- async (runIntCodeChan m ic oc)
  loop ic oc U (0,0) (Map.singleton (0,0) 1) (done comp)
 where
  done = fmap isJust . poll

  loop ic oc dir pos@(x,y) path done =
    done >>= \case
      True -> return path
      False -> do
        writeChan ic $ toInteger $ lookupColor path pos
        c <- fromInteger <$> readChan oc
        let path' = paintColor pos c path
        turn <- readChan oc
        let (dir', pos') = case (dir, turn) of
                            -- turn left
                            (U, 0) -> (L, (x-1,y))
                            (R, 0) -> (U, (x,y-1))
                            (D, 0) -> (R, (x+1,y))
                            (L, 0) -> (D, (x,y+1))
                            -- turn right
                            (U, 1) -> (R, (x+1,y))
                            (R, 1) -> (D, (x,y+1))
                            (D, 1) -> (L, (x-1,y))
                            (L, 1) -> (U, (x,y-1))
        loop ic oc dir' pos' path' done

drawPath :: Int -> Int -> Int -> Int -> Path -> [String]
drawPath minx maxx miny maxy path =
  foldr drawRow [] [miny..maxy]
 where
  drawRow y rs = foldr (drawPixel y) [] [minx..maxx] : rs

  drawPixel y x ps = (maybe ' ' draw $ Map.lookup (x,y) path) : ps

  draw 1 = '#'
  draw _ = ' '

main :: IO ()
main = do
  program <- Vector.fromList
    . map (read . Text.unpack)
    . (Text.split (== ',') . Text.decodeUtf8)
    <$> BS.readFile "day11-input.txt"
  path <- runRobot program
  print $ length path
  printf "x: %d, " $ minimum $ map fst $ Map.keys path
  printf "%d\n" $ maximum $ map fst $ Map.keys path
  printf "y: %d, " $ minimum $ map snd $ Map.keys path
  printf "%d\n" $ maximum $ map snd $ Map.keys path
  putStrLn $ unlines $ drawPath (-10) 50 (-10) 10  path
