#!/usr/bin/env stack
-- stack script --resolver lts-14.4 --package extra,bytestring,text,containers
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TupleSections              #-}
module Main where

import           Control.Monad
import           Data.Function
import           Data.List
import           Data.List.Extra
import           Data.Map        (Map)
import qualified Data.Map        as Map

import           Debug.Trace

type Point = (Float,Float)

newtype Angle = Angle Float deriving Eq

mkAngle :: Float -> Angle
mkAngle f
  | f < 0 = mkAngle $ f + 360
  | f > 360 = mkAngle $ f - 360
  | otherwise = Angle f

instance Ord Angle where
  (Angle a) <= (Angle b) = a <= b

instance Show Angle where
  show (Angle a) = show a

-- | Calculate a custom, clockwise angle
angle :: Point -> Point -> Angle
angle (x1,y1) (x2,y2) = mkAngle $ rad2deg (pi - atan2 (x2-x1) (y2-y1))

asteroidsInView :: [Point] -> Point -> Map Angle [Point]
asteroidsInView ps p1 = foldr go mempty sorted
 where
  sorted = sortBy (compare `on` distance p1) ps

  go p2 = Map.alter (add p2) (angle p1 p2)

  add p2 Nothing   = Just [p2]
  add p2 (Just ps) = Just (p2:ps)

distance :: Point -> Point -> Float
distance (x1,y1) (x2,y2) = sqrt $ (x2-x1) ** 2 + (y2-y1) ** 2

main :: IO ()
main = do
  ls <- lines <$> readFile "day10-input.txt"
  -- let ls = lines $ ".#..#\n\
  --                  \.....\n\
  --                  \#####\n\
  --                  \....#\n\
  --                  \...##"
  -- let ls = lines $ "......#.#.\n\
  --                  \#..#.#....\n\
  --                  \..#######.\n\
  --                  \.#.#.###..\n\
  --                  \.#..#.....\n\
  --                  \..#....#.#\n\
  --                  \#..#....#.\n\
  --                  \.##.#..###\n\
  --                  \##...#..#.\n\
  --                  \.#....####"
  -- let ls = lines $ ".#..#..###\n\
  --                  \####.###.#\n\
  --                  \....###.#.\n\
  --                  \..###.##.#\n\
  --                  \##.##.#.#.\n\
  --                  \....###..#\n\
  --                  \..#.#..#.#\n\
  --                  \#..#.#.###\n\
  --                  \.##...##.#\n\
  --                  \.....#.#.."
  -- let ls = lines $ ".#..##.###...#######\n\
  --                  \##.############..##.\n\
  --                  \.#.######.########.#\n\
  --                  \.###.#######.####.#.\n\
  --                  \#####.##.#.##.###.##\n\
  --                  \..#####..#.#########\n\
  --                  \####################\n\
  --                  \#.####....###.#.#.##\n\
  --                  \##.#################\n\
  --                  \#####.##.###..####..\n\
  --                  \..######..##.#######\n\
  --                  \####.##.####...##..#\n\
  --                  \.#####..#.######.###\n\
  --                  \##...#.##########...\n\
  --                  \#.##########.#######\n\
  --                  \.####.#.###.###.#.##\n\
  --                  \....##.##.###..#####\n\
  --                  \.#.#.###########.###\n\
  --                  \#.#.#.#####.####.###\n\
  --                  \###.##.####.##.#..##"
  let as = foldMap (\(l,y) -> foldr (go y) [] $ zip l [0..]) $ zip ls [0..]
  let (center, inView) = maximumBy (compare `on` (Map.size . snd)) $ map (\p -> (p, asteroidsInView as p)) as
  print center
  putStrLn $ "inView: " ++ show (Map.size inView)
  let vaporizationList = rotateCollect inView
  putStrLn $ "200th: " ++ show (vaporizationList !! 199)
 where
  go y ('#', x) as = (x,y) : as
  go _ _ as        = as

rotateCollect :: Map Angle [Point] -> [Point]
rotateCollect m
  | Map.size m > 0 =
    foldr (\ps xs -> head ps : xs) [] m ++ rotateCollect (Map.mapMaybe pop m)
  | otherwise = []
 where
  pop []     = Nothing
  pop [a]    = Nothing
  pop (a:as) = Just as

clockwise :: Float -> Float
clockwise rad =
  let deg = rad2deg rad
  in  if deg < 0 then deg + 360 else deg

rad2deg :: Float -> Float
rad2deg r = r * 180 / pi
