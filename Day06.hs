#!/usr/bin/env stack
-- stack script --resolver lts-14.4 --package containers
{-# LANGUAGE BangPatterns #-}
module Main where

import           Data.List
import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Data.Set    (Set)
import qualified Data.Set    as Set
import           Debug.Trace

type OrbitMap = Map String (Set String)

parseOrbits :: [String] -> OrbitMap
parseOrbits = foldl' (flip addOrbit) mempty

addOrbit :: String -> OrbitMap -> OrbitMap
addOrbit s = Map.alter add' a
 where
  (a, (_:b)) = break (==')') s

  add' Nothing  = Just $ Set.singleton b
  add' (Just s) = Just $ s <> Set.singleton b

orbitCountChecksum :: OrbitMap -> Int
orbitCountChecksum om = go 0 "COM" 0
 where
  go !depth k !sum =
    case Map.lookup k om of
      Just os -> foldr (go (depth + 1)) (sum + depth) os
      Nothing -> sum + depth

main :: IO ()
main = do
  input <- lines <$> readFile "day06-input.txt"
  -- let input = [ "COM)B"
  --             , "B)C"
  --             , "C)D"
  --             , "D)E"
  --             , "E)F"
  --             , "B)G"
  --             , "G)H"
  --             , "D)I"
  --             , "E)J"
  --             , "J)K"
  --             , "K)L"
  --             ]
  let orbits = parseOrbits input
  -- print orbits
  print $ orbitCountChecksum orbits
