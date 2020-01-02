{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Computer

import Control.Monad.State
import Control.Monad.Identity
import Data.List

import Debug.Trace

data Movement = North | South | West | East
              deriving (Eq, Show)

instance Enum Movement where
  toEnum 1 = North
  toEnum 2 = South
  toEnum 3 = West
  toEnum 4 = East

  fromEnum North = 1
  fromEnum South = 2
  fromEnum West = 3
  fromEnum East = 4

data Status = Wall | Empty | Oxygen
            deriving (Enum, Eq, Show)

step :: IntCode -> Movement -> (Status, IntCode)
step ic m =
  (flip runState) ic $ stepComputer (pure $ toInteger $ fromEnum m) >>= \case
    Nothing -> error "unexpected halt"
    Just o -> return $ toEnum $ fromInteger o

data Node = Node IntCode Int Int Status

instance Eq Node where
  (Node _ x1 y1 s1) == (Node _ x2 y2 s2) = (x1,y1,s1) == (x2,y2,s2)

instance Show Node where
  show (Node _ x y s) = show s ++ " (" ++ show x ++ "," ++ show y ++ ")"

nextNode :: Node -> [Node]
nextNode (Node ic x y _) =
  foldr move [] [North, South, West, East]
 where
  move d ns =
    case step ic d of
      (Wall, _) -> ns
      (s, ic') -> (node ic' s d) : ns

  node ic' s North = Node ic' x (y+1) s
  node ic' s South = Node ic' x (y-1) s
  node ic' s West = Node ic' (x-1) y s
  node ic' s East = Node ic' (x+1) y s

bfs :: Eq a => (a -> [a]) -> (a -> Bool) -> a -> Maybe [a]
bfs next done = go [] . map ([],) . next
 where
  go _ [] = Nothing
  go close ((path, a):open)
    | done a = Just $ reverse (a:path)
    | a `elem` close = go close open
    | otherwise = go (a:close) (open ++ (map (a:path,) $ next a))

main :: IO ()
main = do
  program <- loadIntCode "day15/input.txt"
  -- part one
  print $ fmap length $ bfs nextNode (\(Node _ _ _ s) -> s == Oxygen) $ Node program 0 0 Empty
