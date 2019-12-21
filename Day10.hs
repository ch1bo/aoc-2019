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
import qualified Data.Map        as Map

import           Debug.Trace

type Point = (Float,Float)

angle :: Point -> Point -> Float
angle (x1,y1) (x2,y2) = atan2 (x2-x1) (y2-y1)

los :: [Point] -> Point -> [Point]
los ps p1 = Map.elems $ foldMap (\p2 -> Map.singleton (angle p1 p2) p2) sorted
 where
  sorted = sortBy (compare `on` distance p1) ps

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
  print $ maximumBy (compare `on` fst) $ map (\p -> (length $ los as p, p)) as
 where
  go y ('#', x) as = (x,y) : as
  go _ _ as        = as
