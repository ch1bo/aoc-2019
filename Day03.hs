#!/usr/bin/env stack
-- stack script --resolver lts-14.4 --package extra,containers
{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad
import Data.List.Extra

import qualified Data.Set as Set

type Wire = [Direction]

data Direction = R Int
               | L Int
               | U Int
               | D Int
               deriving Show

-- TODO(SN): instance Read?
parseDirection :: String -> Direction
parseDirection ('R':s) = R $ read s
parseDirection ('L':s) = L $ read s
parseDirection ('U':s) = U $ read s
parseDirection ('D':s) = D $ read s
parseDirection _ = undefined

parseWire :: String -> Wire
parseWire = map parseDirection . split (== ',')

type Coordinate = (Int, Int)

type Path = [Coordinate]

traceWire :: Wire -> Path
traceWire = foldl' go []
 where
  go :: Path -> Direction -> Path
  go p (R n) = trace p (n, 0)
  go p (L n) = trace p (-n, 0)
  go p (U n) = trace p (0, n)
  go p (D n) = trace p (0, -n)

  trace :: Path -> (Int,Int) -> Path
  trace [] delta = trace [(0,0)] delta
  trace p@((x,y):_) (!dx,!dy)
    | dx > 0 = trace ((x+1,y):p) (dx-1,dy)
    | dy > 0 = trace ((x,y+1):p) (dx,dy-1)
    | dx < 0 = trace ((x-1,y):p) (dx+1,dy)
    | dy < 0 = trace ((x,y-1):p) (dx,dy+1)
    | otherwise = p

manhattan :: Coordinate -> Int
manhattan (x,y) = x + y

main :: IO ()
main = do
  (w1:w2:[]) <- lines <$> readFile "day03-input.txt"
  -- let w1 = "R8,U5,L5,D3"
  -- let w2 = "U7,R6,D4,L4"
  -- let w1 = "R75,D30,R83,U83,L12,D49,R71,U7,L72"
  -- let w2 = "U62,R66,U55,R34,D71,R55,D58,R83"
  -- let w1 = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
  -- let w2 = "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
  -- NOTE(SN): init to drop (0,0) origin
  let t1 = init $ traceWire $ parseWire w1
  let t2 = init $ traceWire $ parseWire w2
  print $ length t1
  print $ length t2
  -- Naive brute force search using list monad
  let intersections = do
        c1 <- t1
        c2 <- t2
        guard (c1 == c2)
        return c1
  print intersections
  print $ map manhattan intersections
  print $ Set.findMin $ Set.fromList $ map manhattan intersections
