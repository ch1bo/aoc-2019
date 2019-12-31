#!/usr/bin/env stack
-- stack script --resolver lts-14.4 --package containers,megaparsec
module Main where

import Debug.Trace

import           Data.Void
import           Data.Map (Map)
import           Data.List
import           Data.Foldable
import           Text.Megaparsec
import           Text.Megaparsec.Char

import qualified Data.Map as Map

type Chemical = String

data Reaction = R (Chemical, Int) (Map Chemical Int)

instance Show Reaction where
  show (R c cs) =
    (intercalate ", " $ map showC $ Map.toList cs) ++ " => " ++ showC c
   where
    showC (c,i) = c ++ "(" ++ show i ++ ")"

type Parser = Parsec Void String

parseReaction :: Parser Reaction
parseReaction = do
  cs <- chemical `sepBy` (char ',')
  space >> string "=>"
  c <- chemical
  return $ R c $ Map.fromList cs
 where
  chemical = do
    space
    i <- read <$> some digitChar
    space
    c <- some upperChar
    return $ (c,i)

-- | Fold all reactions from 'FUEL' until 'ORE' and collect required amount of
-- 'ORE' for one 'FUEL'.
requiredOre :: [Reaction] -> Int
requiredOre rs =
  case partition ((== "FUEL") . resultName) rs of
    ([r], rs') -> go r rs'
    _ -> 0
 where
  resultName (R (n,_) _) = n

  onlyOre (R _ is) | Map.size is == 1 = Map.lookup "ORE" is
  onlyOre _ = Nothing

  go r rs =
    case onlyOre r of
      Just x -> x
      Nothing -> go (simplify rs r) rs

simplify :: [Reaction] -> Reaction -> Reaction
simplify rs a = foldr substitute a rs

-- | Substitue the first reaction into intputs of the second reaction.
substitute :: Reaction -> Reaction -> Reaction
-- substitute (R _ as) b | Map.size as == 1 && Map.member "ORE" as = b
substitute (R (an,ac) as) (R br bs) =
  R br $ maybe bs replace $ Map.lookup an bs
 where
  replace req = Map.unionWith (+) (Map.delete an bs) (fmap (convert req ac) as)

  -- convert input amounts for a required chemical, given a produced amount
  convert required produced input
    | required `mod` produced == 0 = required `div` produced * input
    | otherwise = (required `div` produced + 1) * input

main :: IO ()
main = do
  -- input <- lines <$> readFile "day14-input.txt"
  -- let input = "9 ORE => 2 A\n\
  --             \8 ORE => 3 B\n\
  --             \7 ORE => 5 C\n\
  --             \3 A, 4 B => 1 AB\n\
  --             \5 B, 7 C => 1 BC\n\
  --             \4 C, 1 A => 1 CA\n\
  --             \2 AB, 3 BC, 4 CA => 1 FUEL"
  let input = "157 ORE => 5 NZVS\n\
              \165 ORE => 6 DCFZ\n\
              \44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL\n\
              \12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ\n\
              \179 ORE => 7 PSHF\n\
              \177 ORE => 5 HKGWZ\n\
              \7 DCFZ, 7 PSHF => 2 XJWVT\n\
              \165 ORE => 2 GPVTF\n\
              \3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"
  case parse (many parseReaction) "test" input of
    Left e   -> putStrLn $ errorBundlePretty e
    Right rs -> do
      mapM_ print rs
      putStrLn "--------"
      mapM_ print $ take 5 $ iterate (simplify rs) (rs !! 2)
      -- print (requiredOre rs)
