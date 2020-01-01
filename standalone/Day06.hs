#!/usr/bin/env stack
-- stack script --resolver lts-14.4 --package containers
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad
import           Data.List
import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Data.Maybe
import           Data.Set      (Set)
import qualified Data.Set      as Set
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

path :: String -> OrbitMap -> Maybe [String]
path target om = go "COM"
 where
  go :: String -> Maybe [String]
  go k
    | k == target = Just [k]
    | otherwise = do
      os <- Map.lookup k om
      p <- listToMaybe $ catMaybes $ map go $ Set.toList os
      pure $ k:p

-- Calculate required orbital transfers by finding a common orbit
orbitalTransfers :: String -> String -> OrbitMap -> Maybe Int
orbitalTransfers from to om = do
  fp <- path from om
  tp <- path to om
  let cl = length $ commonPrefix fp tp
  pure $ length fp + length tp - 2 * cl - 2
 where
  commonPrefix (a:as) (b:bs)
    | a == b = a:(commonPrefix as bs)
  commonPrefix _ _ = []

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
  --             , "K)L"
  --             , "K)YOU"
  --             , "I)SAN"
  --             ]
  let orbits = parseOrbits input
  print $ orbitCountChecksum orbits
  -- print $ path "YOU" orbits
  -- print $ path "SAN" orbits
  print $ orbitalTransfers "YOU" "SAN" orbits
