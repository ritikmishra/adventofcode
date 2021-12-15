module Day14 where

import Data.List.Split (splitOn)
import Data.Map ((!))
import qualified Data.Map as Map
import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Utils (maybeMap)
import Data.Type.Equality (apply)

data SubRule = SubRule (Char, Char) Char deriving (Eq, Show)

parseInput :: String -> Maybe (String, Map.Map (Char, Char) Char)
parseInput inpStr =
  case splitOn "\n\n" inpStr of
    [polymer, strings] -> do
      rules <- maybeMap parseSubRule (lines strings)
      return (polymer, Map.fromList rules)
    _ -> Nothing
  where
    parseSubRule :: String -> Maybe ((Char, Char), Char)
    parseSubRule str =
      case splitOn " -> " str of
        [[x, y], [c]] -> Just ((x, y), c)
        _ -> Nothing

applyPolychainOnce :: Map.Map (Char, Char) Char -> String -> String
applyPolychainOnce rules polyChain =
  let tupleWindows = zip polyChain $ tail polyChain
   in let mapped = concatMap (\tup@(x, y) -> [x, rules ! tup]) tupleWindows
       in mapped ++ [last polyChain]

count :: String -> Map.Map Char Int
count = foldl (\acc el -> Map.insertWith (+) el 1 acc) Map.empty

------------

stringToFunnyMap :: String -> Map.Map (Char, Char) Int
stringToFunnyMap s =
    let pairs = zip s $ tail s
      in foldl (\acc el -> Map.insertWith (+) el 1 acc) Map.empty pairs

getPairs :: Map.Map (Char, Char) Char -> (Char, Char) -> ((Char, Char), (Char, Char))
getPairs rules p@(a, b) = let c = rules!p in ((a, c), (c, b))

singleStepFunnyMap :: Map.Map (Char, Char) Char -> Map.Map (Char, Char) Int -> Map.Map (Char, Char) Int
singleStepFunnyMap rules = Map.foldrWithKey (\key numOccurences acc ->
        let (p1, p2) = getPairs rules key in
        let acc' = Map.insertWith (+) p1 numOccurences acc
        in Map.insertWith (+) p2 numOccurences acc' ) Map.empty

countCharacters :: Map.Map (Char, Char) Int -> Map.Map Char Int
countCharacters = Map.foldrWithKey (\(a, b) numOccurences acc -> let acc' = Map.insertWith (+) a numOccurences acc
                    in Map.insertWith (+) b numOccurences acc') Map.empty



day14main :: IO ()
day14main = do
  handle <- openFile "../inputs2021/day14.txt" ReadMode
  contents <- hGetContents handle
  case parseInput contents of
    Nothing -> print "could not parse inputs"
    Just (startingChain, rules) -> do
        let lastCharacter = last startingChain
        let fuck = stringToFunnyMap startingChain
        let afterTenSteps = foldl (\acc el -> singleStepFunnyMap rules acc) fuck [1 .. 40]
        let characterCount = Map.insertWith (+) lastCharacter 1 (countCharacters afterTenSteps)
        let nonDoubleCounted = Map.map (`div` 2) characterCount
        print $ maximum nonDoubleCounted - minimum nonDoubleCounted
