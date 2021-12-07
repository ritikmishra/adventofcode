module Day7 where

import System.IO
import Data.List.Split (splitOn)
import Utils (maybeMap)
import Text.Read (readMaybe)

parseToIntList :: String -> Maybe [Int]
parseToIntList commaSeparatedNums =
    let nums = splitOn "," commaSeparatedNums :: [String]
    in maybeMap readMaybe nums

part1cost :: Int -> [Int] -> Int
part1cost newPos curPoses =
    sum (map (abs . subtract newPos) curPoses)

sumToN :: Int -> Int
sumToN n = div (n * (n + 1)) 2

part2cost :: Int -> [Int] -> Int
part2cost newPos curPoses =
    sum (map (sumToN . abs . subtract newPos) curPoses)

bruteForceCheapestPosition :: (Int -> [Int] -> Int) -> [Int] -> (Int, Int)
bruteForceCheapestPosition cost poses =
    let (smallestPos, largestPos) = (minimum poses, maximum poses) in
        let costsByNewPoses = map (\pos -> (cost pos poses, pos)) [smallestPos..largestPos]
        in foldl1 (\acc@(accCost, _) el@(elCost,_) -> if elCost < accCost then el else acc) costsByNewPoses

day7part1 = bruteForceCheapestPosition part1cost 
day7part2 = bruteForceCheapestPosition part2cost

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