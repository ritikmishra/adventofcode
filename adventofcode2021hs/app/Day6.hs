module Day6 where

import qualified Data.Map as Map
import Data.List.Split
import Data.Maybe
import Debug.Trace
import Utils
import Text.Read

import System.IO

parseInitialState :: String -> Maybe (Map.Map Int Int)
parseInitialState str = do
    ages <- (maybeMap readMaybe . splitOn ",") str :: Maybe [Int]
    return $ foldl (\map el -> Map.insertWith (+) el 1 map) Map.empty ages

passOneDay :: Map.Map Int Int -> Map.Map Int Int
passOneDay curState =
    let numReadyFish = fromMaybe 0 (Map.lookup 0 curState)
      in let state' = Map.delete 0 curState
        in let state'' = Map.mapKeys (subtract 1) state'
          in let state''' = Map.insertWith (+) 6 (numReadyFish) state''
            in Map.insertWith (+) 8 numReadyFish state'''

passNdays :: Int -> Map.Map Int Int -> Map.Map Int Int
passNdays nDays state = 
    if nDays == 0 then state
    else passNdays (nDays - 1) (passOneDay state)

countFish :: Map.Map Int Int -> Int
countFish = Map.foldr (+) 0

day6main :: IO ()
day6main = do
    handle <- openFile "../inputs2021/day6.txt" ReadMode
    contents <- hGetContents handle
    case parseInitialState contents of
        Nothing -> putStrLn "could not parse data"
        Just x -> do
            putStrLn "Part 1 Solution: "
            print $ countFish $ passNdays 80 x
            putStrLn "Part 2 Solution: "
            print $ countFish $ passNdays 256 x
