#!/usr/bin/env stack
-- stack script --resolver lts-14.4 --package text,bytestring,safe,vector
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Numeric.Natural
import Control.Monad
import Safe
import Data.Vector ((!?), Vector, (//))
import Data.Foldable

import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector

type Memory = Vector Int

type Address = Int

data Instruction = Add Address Address Address
                 | Multiply Address Address Address
                 | Halt

data IntCode = IntCode { memory :: Memory
                       , pos :: !Address
                       } deriving Show

parseInstruction :: IntCode -> Maybe (Instruction, IntCode)
parseInstruction i = do
  (op, i') <- parseInt i
  case op of
    1 -> do
      (a, i'') <- parseInt i'
      (b, i''') <- parseInt i''
      (r, i'''') <- parseInt i'''
      Just (Add a b r, i'''') -- TODO validate address ranges?
    2 -> do
      (a, i'') <- parseInt i'
      (b, i''') <- parseInt i''
      (r, i'''') <- parseInt i'''
      Just (Multiply a b r, i'''') -- TODO validate address ranges?
    99 -> Just (Halt, i')
    _ -> Nothing

parseInt :: IntCode -> Maybe (Int, IntCode)
parseInt i = (, i { pos = pos i + 1 }) <$> lookupInt (pos i) i

lookupInt :: Address -> IntCode -> Maybe Int
lookupInt a i = memory i !? a

runIntCode :: Memory -> Maybe IntCode
runIntCode is = go $ IntCode is 0
 where
  go i = do
    (op, i') <- parseInstruction i
    case op of
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
  -- Naive search / brute force search for a solution using permutations the list monad
  print $ do
    noun <- [0..99]
    verb <- [0..99]
    guard (compute gravityAssistProgram noun verb == Just 19690720)
    return (noun, verb, 100 * noun + verb)
