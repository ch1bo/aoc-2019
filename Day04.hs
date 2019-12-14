#!/usr/bin/env stack
-- stack script --resolver lts-14.4
module Main where

import           Control.Monad
import           Data.List

type Password = Int

check :: Password -> Bool
check p = doubles p && increasing p

doubles :: Password -> Bool
doubles p = any ((==2) . length) $ group $ show p

increasing :: Password -> Bool
increasing p = maybe False (const True) $ foldl' go (Just '0') $ show p
  where
  go (Just p) c | c >= p = Just c
  go _ _        = Nothing

main :: IO ()
main = print $ length $ filter check [136818..685979]
