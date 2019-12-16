#!/usr/bin/env stack
-- stack script --resolver lts-14.4 --package containers
{-# LANGUAGE BangPatterns #-}
module Main where

import           Data.List
import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Debug.Trace

data OrbitMap = Orbits String [OrbitMap]
              deriving Show

parseOrbits :: [String] -> OrbitMap
parseOrbits = foldl' (flip addOrbit) (Orbits "COM" [])

addOrbit :: String -> OrbitMap -> OrbitMap
addOrbit s (Orbits n os)
  | n == a = Orbits a os'
  | otherwise = Orbits n (fmap (addOrbit s) os) -- TODO Foldable OrbitMap
 where
  (a, (_:b)) = break (==')') s

  os' = maybe (Orbits b []:os) (const os) $ find (\(Orbits n _) -> n == b) os

orbitCountChecksum :: OrbitMap -> Int
orbitCountChecksum o = go 0 o 0
 where
  go depth (Orbits n []) acc = (trace $ (show depth) ++ " " ++ n ++ " " ++ (show acc)) acc + depth
  go depth (Orbits n os) acc = (trace $ (show depth) ++ " " ++ n ++ " " ++ (show acc)) foldr (go $ depth + 1) (acc + depth) os

main :: IO ()
main = do
  -- TODO(SN): input not sorted!
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
  print orbits
  print $ orbitCountChecksum orbits
