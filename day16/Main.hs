{-# LANGUAGE BangPatterns #-}
module Main where

pattern :: Int -- ^ Position
        -> [Int]
pattern n = concat [ replicate n 0
                   , replicate n 1
                   , replicate n 0
                   , replicate n (-1)
                   ]

apply :: Int -- ^ Position
      -> [Int] -> Int
apply n input = oneDigit $ sum input (tail pat)
 where
  pat :: [Int]
  pat = pattern n

  sum [] _          = 0
  sum is []         = sum is pat
  sum (i:is) (p:ps) = i*p + sum is ps

  oneDigit = flip mod 10 . abs

phase :: [Int] -> [Int]
phase is = map (`apply` is) [1..(length is)]

runPhases :: Int -> [Int] -> [[Int]]
runPhases n input = take n $ iterate phase (phase input)

main :: IO ()
main = do
  -- part one
  -- let input = [1,2,3,4,5,6,7,8]
  -- print $ runPhases 4 input
  -- let input = [8,0,8,7,1,2,2,4,5,8,5,9,1,4,5,4,6,6,1,9,0,8,3,2,1,8,6,4,5,5,9,5]
  input <- map (read . (:[])) . head . lines <$> readFile "day16/input.txt"
  putStrLn . concatMap show . take 8 . last $ runPhases 100 input
