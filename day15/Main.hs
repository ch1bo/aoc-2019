module Main where

import Computer (loadProgram, runComputer)

main :: IO ()
main = do
  program <- loadProgram "day15/input.txt"
  _ <- runComputer program
