#!/usr/bin/env stack
-- stack script --resolver lts-14.4 --package extra
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TupleSections              #-}
module Main where

import           Data.Function
import           Data.List
import           Data.List.Extra

type Layer = [Char]

loadImage :: Int -> Int -> [Char] -> [Layer]
loadImage width height = filter (\l -> length l == n) . chunksOf n
 where
  -- go [] = []
  -- go ps
  --   | length ps >= n = take n ps : go (drop n ps)
  --   | otherwise = []

  n = width * height

count :: Eq a => a -> [a] -> Int
count a = length . filter (== a)

combine :: [Layer] -> Layer
combine []       = error "no layers to combine"
combine [l]      = l
combine (l1:l2:ls) = combine $ combineL l1 l2 : ls
 where
  combineL [] []         = []
  combineL (x:xs) (y:ys) = (combineP x y) : (combineL xs ys)

  combineP '2' '2' = '2'
  combineP '2' '0' = '0'
  combineP '2' '1' = '1'
  combineP '0' _   = '0'
  combineP '1' _   = '1'

showLayer :: Int -> Layer -> String
showLayer w ps = intercalate "\n" $ chunksOf w ps

main :: IO ()
main = do
  image <- readFile "day08-input.txt"
  let ls = loadImage 25 6 image
  let l = minimumBy (compare `on` (count '0')) ls
  print $ count '1' l * count '2' l
  putStrLn . showLayer 25 $ combine ls
