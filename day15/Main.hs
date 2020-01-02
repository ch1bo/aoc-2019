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

data Status = Wall | Moved | Found
            deriving (Enum, Show)

step :: IntCode -> Movement -> (Status, IntCode)
step ic m =
  (flip runState) ic $ stepComputer (pure $ toInteger $ fromEnum m) >>= \case
    Nothing -> error "unexpected halt"
    Just o -> return $ toEnum $ fromInteger o

type Path = [Movement]

next :: Path -> [Path]
next [] = [North:[], South:[], West:[], East:[]]
next p@(North:_) = [North:p, West:p, East:p]
next p@(South:_) = [South:p, West:p, East:p]
next p@(West:_) = [West:p, North:p, South:p]
next p@(East:_) = [East:p, North:p, South:p]

bfs :: IntCode -> Maybe [Movement]
bfs ic = go $ map (ic,) $ next []
 where
  go [] = Nothing
  go ((_, []):open) = go open
  go ((ic, path):open) =
    case step ic $ head path of
      (Found, _) -> Just path
      (Moved, ic') -> go $ open ++ (map (ic',) $ next path)
      (Wall, _) -> go open

main :: IO ()
main = do
  program <- loadIntCode "day15/input.txt"
  -- part one
  print $ length <$> bfs program
