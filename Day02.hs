#!/usr/bin/env stack
-- stack script --resolver lts-14.4 --package text,bytestring,safe,vector,mtl
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
module Main where

import           Control.Monad
import           Control.Monad.State
import           Data.Foldable
import           Data.Vector         (Vector, (!?), (//))
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
                 | Halt

data IntCode = IntCode { memory :: Memory
                       , pos    :: !Address
                       } deriving Show

parseInstruction :: State IntCode (Maybe Instruction)
parseInstruction = do
  op <- parseInt
  case op of
    Just 1 -> do
      a <- parseInt
      b <- parseInt
      r <- parseInt
      return $ Add <$> a <*> b <*> r -- TODO validate address ranges?
    Just 2 -> do
      a <- parseInt
      b <- parseInt
      r <- parseInt
      return $ Multiply <$> a <*> b <*> r -- TODO validate address ranges?
    Just 99 -> return $ Just Halt
    _ -> return Nothing

parseInt :: State IntCode (Maybe Int)
parseInt = do
  i <- get
  put (i { pos = pos i + 1})
  return $ lookupInt (pos i) i

lookupInt :: Address -> IntCode -> Maybe Int
lookupInt a i = memory i !? a

runIntCode :: Memory -> Maybe IntCode
runIntCode is = go $ IntCode is 0
 where
  go i = do
    let (op, i') = runState parseInstruction i
    op >>= \case
      Add a b r -> do
        x <- lookupInt a i
        y <- lookupInt b i
        go $ i' { memory = memory i' // [(r, x + y)] }
      Multiply a b r -> do
        x <- lookupInt a i
        y <- lookupInt b i
        go $ i' { memory = memory i' // [(r, x * y)] }
      Halt -> pure i'

type Noun = Int
type Verb = Int

compute :: Memory -> Noun -> Verb -> Maybe Int
compute m n v = runIntCode (m // [(1, n), (2, v)]) >>= lookupInt 0

main :: IO ()
main = do
  gravityAssistProgram <- Vector.fromList . map (read . Text.unpack) . (Text.split (== ',') . Text.decodeUtf8) <$> BS.readFile "day02-input.txt"
  print $ compute gravityAssistProgram 1 1
  -- Naive search / brute force search for a solution using permutations the list monad
  print $ do
    noun <- [0..99]
    verb <- [0..99]
    guard (compute gravityAssistProgram noun verb == Just 19690720)
    return (noun, verb, 100 * noun + verb)
