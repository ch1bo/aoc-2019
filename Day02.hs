#!/usr/bin/env stack
-- stack script --resolver lts-14.4 --package text,bytestring,safe,vector
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Numeric.Natural
import Safe
import Data.Vector ((!?), Vector, (//))

import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector

type Address = Int

data OpCode = Add Address Address Address
            | Multiply Address Address Address
            | Halt

data IntCode = IntCode { code :: Vector Int
                       , pos :: !Address
                       } deriving Show

parseOpCode :: IntCode -> Maybe (OpCode, IntCode)
parseOpCode i = do
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
lookupInt a i = code i !? a

runIntcode :: Vector Int -> Maybe IntCode
runIntcode is = go $ IntCode is 0
 where
  go i = do
    (op, i') <- parseOpCode i
    case op of
      Add a b r -> do
        x <- lookupInt a i
        y <- lookupInt b i
        go $ i' { code = code i' // [(r, x + y)] }
      Multiply a b r -> do
        x <- lookupInt a i
        y <- lookupInt b i
        go $ i' { code = code i' // [(r, x * y)] }
      Halt -> pure i'

main :: IO ()
main = do
  ints <- Vector.fromList . map (read . Text.unpack) . (Text.split (== ',') . Text.decodeUtf8) <$> BS.readFile "day02-input.txt"
  print $ runIntcode (ints // [(1, 12), (2, 2)]) >>= lookupInt 0
