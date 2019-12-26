#!/usr/bin/env stack
-- stack script --resolver lts-14.4
module Main where

import Data.List

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

simulate :: Int -> [Moon] -> [Moon]
simulate n ms = iterate simulateStep ms !! n

totalEnergy :: [Moon] -> Int
totalEnergy = foldr energy 0
 where
  energy m e = e + kin m * pot m

  pot m = abs (posX m) + abs (posY m) + abs (posZ m)

  kin m = abs (velX m) + abs (velY m) + abs (velZ m)

main :: IO ()
main = do
  print $ totalEnergy $ simulate 1000 ms
 where
  ms = [m1,m2,m3,m4]
  m1 = mkMoon 4 1 1
  m2 = mkMoon 11 (-18) (-1)
  m3 = mkMoon (-2) (-10) (-4)
  m4 = mkMoon (-7) (-2) 14
