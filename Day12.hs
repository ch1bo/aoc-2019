#!/usr/bin/env stack
-- stack script --resolver lts-14.4 --package unordered-containers
{-# LANGUAGE BangPatterns #-}
module Main where

import Debug.Trace

import Data.List
import Data.Maybe
import Data.Foldable

import qualified Data.HashMap.Lazy as Map

data Moon = Moon { posX :: Int
                 , posY :: Int
                 , posZ :: Int
                 , velX :: Int
                 , velY :: Int
                 , velZ :: Int
                 } deriving (Eq, Show)

mkMoon :: Int -> Int -> Int -> Moon
mkMoon x y z = Moon x y z 0 0 0

-- | Apply gravity of a moon on to another moon. The velocities of the second
-- moon are updated and returned.
applyGravity :: Moon -> Moon -> Moon
applyGravity a b =
  b { velX = velX b + (signum (posX a - posX b))
    , velY = velY b + (signum (posY a - posY b))
    , velZ = velZ b + (signum (posZ a - posZ b))
    }

applyVelocity :: Moon -> Moon
applyVelocity m = m { posX = posX m + velX m
                    , posY = posY m + velY m
                    , posZ = posZ m + velZ m
                    }

simulateStep :: [Moon] -> [Moon]
simulateStep ms =
  map applyVelocity $
  [ foldr applyGravity m (ms \\ [m]) | m <- ms ]

totalEnergy :: [Moon] -> Int
totalEnergy = foldr energy 0
 where
  energy m e = e + kin m * pot m

  pot m = abs (posX m) + abs (posY m) + abs (posZ m)

  kin m = abs (velX m) + abs (velY m) + abs (velZ m)

periods :: [[Moon]] -> (Int,Int,Int)
periods [] = (0,0,0)
periods (m0:ms) = go 1 (Nothing,Nothing,Nothing) ms
 where
  go _ (Just px, Just py, Just pz) _ = (px,py,pz)
  go _ _ [] = (0,0,0)
  go !i (px,py,pz) (m:ms) =
    go (i+1) ( if atStartX m then Just (fromMaybe i px) else px
             , if atStartY m then Just (fromMaybe i py) else py
             , if atStartZ m then Just (fromMaybe i pz) else pz
             ) ms

  atStartX m = all (\(a,a0) -> velX a == 0 && posX a == posX a0) $ zip m m0
  atStartY m = all (\(a,a0) -> velY a == 0 && posY a == posY a0) $ zip m m0
  atStartZ m = all (\(a,a0) -> velZ a == 0 && posZ a == posZ a0) $ zip m m0

main :: IO ()
main = do
  -- part one
  print $ totalEnergy $ iterate simulateStep ms !! 1000
  -- part two
  let (a,b,c) = periods $ iterate simulateStep ms
  print (a,b,c)
  print $ lcm (lcm a b) c
 where
  ms = [m1,m2,m3,m4]
  m1 = mkMoon 4 1 1
  m2 = mkMoon 11 (-18) (-1)
  m3 = mkMoon (-2) (-10) (-4)
  m4 = mkMoon (-7) (-2) 14

  -- m1 = mkMoon (-1) 0 2
  -- m2 = mkMoon 2 (-10) (-7)
  -- m3 = mkMoon 4 (-8) 8
  -- m4 = mkMoon 3 5 (-1)

  -- m1 = mkMoon (-8) (-10) 0
  -- m2 = mkMoon 5 5 10
  -- m3 = mkMoon 2 (-7) 3
  -- m4 = mkMoon 9 (-8) (-3)
