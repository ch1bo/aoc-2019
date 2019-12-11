#!/usr/bin/env stack
-- stack script --resolver lts-14.4
module Main where

import Numeric.Natural

type Mass = Natural
type Fuel = Natural

fuelCounter :: Mass -> Fuel
fuelCounter m = (m `div` 3) - 2

main :: IO ()
main = do
  modules <- fmap read . lines <$> readFile "input.txt"
  print $ sum $ map fuelCounter modules
