module Day7 where

import Data.List.Split (splitOn)
import System.IO
import Text.Read (readMaybe)
import Utils (maybeMap)

parseToIntList :: String -> Maybe [Int]
parseToIntList commaSeparatedNums =
  let nums = splitOn "," commaSeparatedNums :: [String]
   in maybeMap readMaybe nums

sumToN :: Int -> Int
sumToN n = div (n * (n + 1)) 2

findLowestCost :: (Int -> Int) -> [Int] -> Int
findLowestCost cost poses = minimum [sum [cost (curPose - newPose) | curPose <- poses] | newPose <- [0 .. 1000]]

day7part1 = findLowestCost abs

day7part2 = findLowestCost $ abs . sumToN

day7main :: IO ()
day7main = do
  handle <- openFile "../inputs2021/day7.txt" ReadMode
  contents <- hGetContents handle
  case parseToIntList contents of
    Nothing -> putStrLn "could not parse"
    Just x -> do
      print $ day7part1 x
      print $ day7part2 x
  return ()