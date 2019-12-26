#!/usr/bin/env stack
-- stack script --resolver lts-14.4 --package unordered-containers
{-# LANGUAGE BangPatterns #-}
module Main where

import Debug.Trace

import Data.List
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

main :: IO ()
main = do
  -- part one
  print $ totalEnergy $ iterate simulateStep ms !! 1000
  -- part two
  print $ foldlM twoSame (0,Map.empty) $ iterate simulateStep ms
 where
  twoSame (!i,m) ms =
    let e = totalEnergy ms
    in case Map.lookup e m of
      Just ms' | any (== ms) ms' -> Left i
      _ -> if i `mod` 1000 == 0
           then trace (show i) Right (i+1,Map.alter (prepend ms) e m)
           else Right (i+1,Map.alter (prepend ms) e m)

  prepend a Nothing = Just [a]
  prepend a (Just as) = Just (a:as)

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
