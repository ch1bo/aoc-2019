#!/usr/bin/env stack
-- stack script --resolver lts-14.4 --package containers,megaparsec
module Main where

import Debug.Trace

import           Data.Void
import           Data.Map (Map)
import           Data.List
import           Data.Maybe
import           Data.Foldable
import           Text.Megaparsec
import           Text.Megaparsec.Char

import qualified Data.Map as Map

type Chemical = String

data Reaction = R { resultName :: Chemical
                  , resultCount :: Int
                  , inputs :: Map Chemical Int
                  } deriving Eq

instance Show Reaction where
  show r =
    (intercalate ", " $ map showC $ Map.toList (inputs r)) ++ " => " ++ showC (resultName r, resultCount r)
   where
    showC (c,i) = c ++ "(" ++ show i ++ ")"

instance Ord Reaction where
  (R _ _ as) <= (R _ _ bs) = Map.size as <= Map.size bs

type Parser = Parsec Void String

parseReaction :: Parser Reaction
parseReaction = do
  cs <- chemical `sepBy` (char ',')
  space >> string "=>"
  (c,i) <- chemical
  return $ R c i $ Map.fromList cs
 where
  chemical = do
    space
    i <- read <$> some digitChar
    space
    c <- some upperChar
    return $ (c,i)

-- | Simplify reaction to 'FUEL' until only 'ORE' is in the inputs.
requiredOre :: [Reaction] -> Int
requiredOre rs = fromMaybe 0 $ do
  r <- lookupFuel rs
  onlyOre r <|> Just (requiredOre $ simplifyInputs r)
 where
  lookupFuel = find ((== "FUEL") . resultName)

  simplifyInputs (R _ _ is) = foldr (\(c,_) xs -> simplify c xs) rs $ Map.toList is

  onlyOre (R _ _ is) | Map.size is == 1 = Map.lookup "ORE" is
  onlyOre _ = Nothing

  -- go [("ORE", x)] rs = x
  go ((c,_):cs) rs =
    go cs $ simplify c rs
  go x rs = trace ("go " ++ show x) 0

-- | Simplify by finding all free reactions and substitute them.
simplify :: Chemical -> [Reaction] -> [Reaction]
simplify _ [] = []
simplify n rs =
  case findFree n of
    Just fr -> map (substitute fr) $ rs \\ [fr]
    Nothing -> rs
 where
  -- | A 'free' reaction is only used in one reaction as input.
  isFree r = 1 == length (filter (\(R _ _ is) -> Map.member (resultName r) is) rs)

  findFree n = find (\r -> resultName r == n && isFree r) rs

-- | Substitue the first reaction into intputs of the second reaction.
substitute :: Reaction -> Reaction -> Reaction
substitute a@(R an ac as) b@(R bn bc bs) = fromMaybe b $ do
  req <- Map.lookup an bs -- required amount of chemical a in inputs of b
  return $ R bn bc $ replace req
 where
  replace req = Map.unionWith (+) (Map.delete an bs) (fmap (convert req ac) as)

  -- convert input amounts for a required chemical, given a produced amount
  convert required produced input
    | required `mod` produced == 0 = required `div` produced * input
    | otherwise = (required `div` produced + 1) * input

main :: IO ()
main = do
  -- input <- readFile "day14-input.txt"
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
  -- let input = "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG\n\
  --             \17 NVRVD, 3 JNWZP => 8 VPVL\n\
  --             \53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL\n\
  --             \22 VJHF, 37 MNCFX => 5 FWMGM\n\
  --             \139 ORE => 4 NVRVD\n\
  --             \144 ORE => 7 JNWZP\n\
  --             \5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC\n\
  --             \5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV\n\
  --             \145 ORE => 6 MNCFX\n\
  --             \1 NVRVD => 8 CXFTF\n\
  --             \1 VJHF, 6 MNCFX => 4 RFSQX\n\
  --             \176 ORE => 6 VJHF"
  case parse (sepEndBy1 parseReaction newline) "test" input of
    Left e   -> putStrLn $ errorBundlePretty e
    Right rs -> do
      mapM_ print rs
      putStrLn "---part one-----"
      print (requiredOre rs)
