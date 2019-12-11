#!/usr/bin/env stack
-- stack script --resolver lts-14.4
{-# LANGUAGE BangPatterns#-}
module Main where

import Numeric.Natural

type Module = Natural
type Mass = Natural
type Fuel = Natural

requiredFuel :: Mass -> Fuel
requiredFuel !m
  | m' >= 2 = (m' - 2) + requiredFuel (m' - 2)
  | otherwise = 0
 where
  m' = m `div` 3

moduleFuel :: Module -> Fuel
moduleFuel m = requiredFuel m

main :: IO ()
main = do
  modules <- fmap read . lines <$> readFile "input.txt"
  print $ sum $ map moduleFuel (modules :: [Module])
