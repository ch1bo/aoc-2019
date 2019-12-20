#!/usr/bin/env stack
-- stack script --resolver lts-14.4 --package text,bytestring,vector,mtl,async
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TupleSections              #-}
module Main where

import           Data.Function
import           Data.List

type Layer = [Char]

loadImage :: Int -> Int -> [Char] -> [Layer]
loadImage width height = go
 where
  go [] = []
  go ps
    | length ps >= n = take n ps : go (drop n ps)
    | otherwise = []

  n = width * height

count :: Eq a => a -> [a] -> Int
count a = length . filter (== a)

main :: IO ()
main = do
  image <- readFile "day08-input.txt"
  let ls = loadImage 25 6 image
  -- print $ length ls
  let l = minimumBy (compare `on` (count '0')) ls
  print $ count '1' l * count '2' l
