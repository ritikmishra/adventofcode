{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day3 where

import Data.Bifunctor
import Data.Bits
import Data.List
import System.IO

data Binary = Zero | One deriving (Show, Eq)

parseStringIntoBinary :: String -> Maybe [Binary]
parseStringIntoBinary "" = Just []
parseStringIntoBinary ('0' : xs) = do
  rest <- parseStringIntoBinary xs
  return $ Zero : rest
parseStringIntoBinary ('1' : xs) = do
  rest <- parseStringIntoBinary xs
  return $ One : rest
parseStringIntoBinary (x : xs) = Nothing

maybeMap :: (a -> Maybe b) -> [a] -> Maybe [b]
maybeMap f [] = Just []
maybeMap f (x : xs) = do
  first <- f x
  rest <- maybeMap f xs
  return $ first : rest

-- Apply Not to a single bit
bitNot :: Binary -> Binary
bitNot Zero = One
bitNot One = Zero

-- Do a bitwise not operation on a whole binary string
bitwiseNot :: [Binary] -> [Binary]
bitwiseNot = map bitNot

-- Convert a binary string to an integer
binaryStrToInt :: [Binary] -> Int
binaryStrToInt = binaryStrToInt' . reverse
  where
    binaryStrToInt' lst =
      case lst of
        [] -> 0
        Zero : xs -> binaryStrToInt' xs * 2
        One : xs -> binaryStrToInt' xs * 2 + 1

-- Enumerate the elements of a list
enumerate :: [a] -> [(Int, a)]
enumerate x =
  enumerate' 0 x
  where
    enumerate' i [] = []
    enumerate' i (x : xs) =
      (i, x) : enumerate' (i + 1) xs

-- Get the most common bit in a binary string
getMostCommonBit :: [Binary] -> Binary
getMostCommonBit num =
  let (zeros, ones) =
        foldl
          ( \(zeros, ones) el ->
              ( case el of
                  Zero -> (zeros + 1, ones)
                  One -> (zeros, ones + 1)
              )
          )
          (0, 0)
          num
   in if zeros > ones then Zero else One

day1part1 :: [[Binary]] -> Int
day1part1 nums =
  let transposed = transpose nums
   in let mostCommonBit = map getMostCommonBit transposed
       in let leastCommonBit = bitwiseNot mostCommonBit
           in let (gamma, epsilon) = (binaryStrToInt mostCommonBit, binaryStrToInt leastCommonBit)
               in gamma * epsilon

day1part2 :: [[Binary]] -> Int
day1part2 binary =
  let mapStratToBestBitIdx = (day1part2' . enumerate) binary
   in let mapBitIdxToNumber = binaryStrToInt . (binary !!)
       in let bestNumberByStrat = mapBitIdxToNumber . mapStratToBestBitIdx
           in let oxy = bestNumberByStrat getMostCommonBit
               in let co2 = bestNumberByStrat (bitNot . getMostCommonBit)
                   in oxy * co2
  where
    day1part2' :: [(Int, [Binary])] -> ([Binary] -> Binary) -> Int
    day1part2' binary strat =
      let mostCommonHeadBit = strat $ map (head . snd) binary
       in let strsWithMostCommonHeadBit = filter ((\(x : xs) -> x == mostCommonHeadBit) . snd) binary
           in case strsWithMostCommonHeadBit of
                [] -> error "bruh what the hell"
                [(i, _)] -> i
                -- second tail maps the second part of the tuple
                remaining -> day1part2' (map (second tail) remaining) strat

day3main :: IO ()
day3main = do
  handle <- openFile "../inputs2021/day3.txt" ReadMode
  contents <- hGetContents handle
  let binaryNums = maybeMap parseStringIntoBinary (lines contents) :: Maybe [[Binary]]
  case binaryNums of
    Nothing -> do
      print "bruh couldn't read the binary"
      return ()
    Just nums -> do
      let powerConsumption = day1part1 nums
      let lifeSupportRating = day1part2 nums
      putStr "Power consumption (part 1): "
      print powerConsumption
      putStr "Life support rating (part 2): "
      print lifeSupportRating
      return ()
