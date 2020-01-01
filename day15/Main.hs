{-# LANGUAGE LambdaCase #-}
module Main where

import Computer

import Control.Monad.State
import Control.Monad.Identity

data Movement = North | South | West | East
              deriving Show

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

type RC = StateT IntCode IO

step :: Movement -> RC Status
step m =
  stepComputer (pure $ toInteger $ fromEnum m) >>= \case
    Nothing -> error "unexpected halt"
    Just o -> return $ toEnum $ fromInteger o

main :: IO ()
main = do
  program <- loadIntCode "day15/input.txt"
  flip evalStateT program $ do
    step North >>= liftIO . print
    step North >>= liftIO . print
    step North >>= liftIO . print
    step North >>= liftIO . print
    step North >>= liftIO . print
    step North >>= liftIO . print
    step North >>= liftIO . print
    step North >>= liftIO . print
