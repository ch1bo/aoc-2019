#!/usr/bin/env stack
-- stack script --resolver lts-14.4 --package extra,containers
{-# LANGUAGE BangPatterns #-}
module Main where

import           Control.Monad
import           Data.List.Extra
import           Data.Map        (Map)
import           Data.Maybe
import           Numeric.Natural

import qualified Data.Map        as Map

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
parseDirection _       = undefined

parseWire :: String -> Wire
parseWire = map parseDirection . split (== ',')

type Coordinate = (Int, Int)

type Path = [Coordinate]

traceWire :: Wire -> Path
traceWire = foldl' go [(0,0)]
 where
  go :: Path -> Direction -> Path
  go p (R n) = traceX p n
  go p (L n) = traceX p (-n)
  go p (U n) = traceY p n
  go p (D n) = traceY p (-n)

  traceX :: Path -> Int -> Path
  traceX p@((x,y):_) !dx
    | dx > 0 = traceX ((x+1,y):p) (dx-1)
    | dx < 0 = traceX ((x-1,y):p) (dx+1)
    | otherwise = p
  traceX p _ = p

  traceY :: Path -> Int -> Path
  traceY p@((x,y):_) !dy
    | dy > 0 = traceY ((x,y+1):p) (dy-1)
    | dy < 0 = traceY ((x,y-1):p) (dy+1)
    | otherwise = p
  traceY p _ = p

manhattan :: Coordinate -> Natural
manhattan (x,y) = toEnum $ abs x + abs y

collect :: [Coordinate] -> Map Natural [Coordinate]
collect = foldl' go mempty
 where
  go m c = let k = manhattan c
           in  Map.alter (f k c) k m

  f k c Nothing   = Just [c]
  f k c (Just cs) = Just (c:cs)

-- Naive brute force search using list monad
intersections :: [Coordinate] -> [Coordinate] -> [Coordinate]
intersections t1 t2 = do
  c1 <- t1
  c2 <- t2
  guard (c1 == c2)
  return c1

main :: IO ()
main = do
  (w1:w2:[]) <- lines <$> readFile "day03-input.txt"
  -- let w1 = "R8,U5,L5,D3"
  -- let w2 = "U7,R6,D4,L4"
  -- let w1 = "R75,D30,R83,U83,L12,D49,R71,U7,L72"
  -- let w2 = "U62,R66,U55,R34,D71,R55,D58,R83"
  -- let w1 = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
  -- let w2 = "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
  let t1 = reverse $ traceWire $ parseWire w1
  let t2 = reverse $ traceWire $ parseWire w2

  -- Group by manhattan distance, NOTE(SN): init to drop (0,0) origin
  let m1 = collect $ tail t1
  let m2 = collect $ tail t2
  print $ Map.toList $ Map.filter notNull $ Map.unionWith intersections m1 m2

  let is = foldMap id $ Map.unionWith intersections m1 m2
  print $ head $ sort $ catMaybes $ map (\c -> (+) <$> elemIndex c t1 <*> elemIndex c t2) is
