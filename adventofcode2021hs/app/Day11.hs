{-# LANGUAGE LambdaCase #-}

module Day11 where

import Data.Char (digitToInt)
import qualified Data.Map as Map
import System.IO

data Octopus
  = Flashed
  | Energy Int deriving (Eq, Show)

parseOctopusgrid :: String -> Map.Map (Int, Int) Octopus
parseOctopusgrid input =
  let enumeratedlines = zip [0 ..] $ lines input
   in let indexedOctos = concatMap (\(row, str) -> [((row, col), Energy $ digitToInt char) | (col, char) <- zip [0 ..] str]) enumeratedlines
       in Map.fromList indexedOctos

adjacentIndexes :: (Int, Int) -> [(Int, Int)]
adjacentIndexes (r, c) = [(r + rowDelta, c + colDelta) | rowDelta <- [-1, 0, 1], colDelta <- [-1, 0, 1]]

increaseEnergy :: Octopus -> Octopus
increaseEnergy Flashed = Flashed
increaseEnergy (Energy x) = Energy (x + 1) -- Flashed is like having energy 0

unmarkFlashed :: Octopus -> Octopus
unmarkFlashed Flashed = Energy 0
unmarkFlashed (Energy x) = Energy x

-- Do a single step of the dumbo octopus dance
singleIteration flashCount octopi  =
  let (flashCount', octopi') = flashEachIdx (flashCount, increaseAllEnergyBy1 octopi)
     in (flashCount', unmarkAllFlashed octopi')
  where
    increaseAllEnergyBy1 = Map.map increaseEnergy
    -- Flash the octopus at the given index, updating all of its neighbors in the process
    flashIndex :: (Int, Int) -> Map.Map (Int, Int) Octopus -> Map.Map (Int, Int) Octopus
    flashIndex idx octopi =
      let octopi' = Map.insert idx Flashed octopi
       in foldl (flip (Map.adjust increaseEnergy)) octopi' $ adjacentIndexes idx
    
    -- For each octopus, flash it if appropriate
    flashEachIdx :: (Int, Map.Map (Int, Int) Octopus) -> (Int, Map.Map (Int, Int) Octopus)
    flashEachIdx t@(flashCount, octopi) =
      let t'@(flashCount', eachMaybeFlashed) =
            Map.foldrWithKey
              ( \key val t'@(flashCount', octopi') -> 
                  case val of
                    Energy x -> if x > 9 then (flashCount' + 1, flashIndex key octopi') else t'
                    Flashed -> t'
              )
              t
              octopi
       in if any (\case Energy x -> x > 9; Flashed -> False) eachMaybeFlashed then flashEachIdx t' else t'
    
    -- Reset all flashed octopi to 0 energy at the end of an iteration
    unmarkAllFlashed = Map.map unmarkFlashed

-- Take 100 steps and count the number of flashes on that step
day11part1 :: Map.Map (Int, Int) Octopus -> Int
day11part1 octopi = 
    fst $ foldl (\acc el -> uncurry singleIteration acc) (0, octopi) [1..100]

-- Take steps until we hit a step where there are 100 flashes. Then, return the step number
day11part2 :: Int -> Map.Map (Int, Int) Octopus -> Int
day11part2 dayCount octopi = 
    let (flashCount, octopi') = singleIteration 0 octopi
      in if flashCount == 100 then dayCount + 1 else day11part2 (dayCount + 1) octopi'

day11main :: IO ()
day11main = do
  handle <- openFile "../inputs2021/day11.txt" ReadMode
  contents <- hGetContents handle
  let grid = parseOctopusgrid contents
  putStr "Day 11 part 1:"
  print $ day11part1 grid
  putStr "Day 11 part 2:"
  print $ day11part2 0 grid
  return ()