#!/usr/bin/env stack
-- stack script --resolver lts-14.4
{-# LANGUAGE BangPatterns#-}
module Main where

import Numeric.Natural

type Module = Natural
type Mass = Natural
type Fuel = Natural

requiredFuel :: Mass -> Fuel
requiredFuel m
  | m' >= 2 = m' - 2
  | otherwise = 0
 where
  m' = m `div` 3

moduleFuel :: Module -> Fuel
moduleFuel = go
 where
  go 0 = 0
  go !x = requiredFuel x + go (requiredFuel x)

main :: IO ()
main = do
  modules <- fmap read . lines <$> readFile "day01-input.txt"
  print $ sum $ map moduleFuel (modules :: [Module])
