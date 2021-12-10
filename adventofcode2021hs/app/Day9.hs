{-# LANGUAGE LambdaCase #-}

module Day9 where

import Data.Char (digitToInt)
import Data.List (sortBy)
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.IO

parseToArrayOfArrays :: String -> Map.Map (Int, Int) Int
parseToArrayOfArrays input =
  let enumeratedLines = zip [0 ..] (lines input)
   in let foo = [((i, j), digit) | (i, line) <- enumeratedLines, (j, digit) <- parseLine line]
       in Map.fromList foo
  where
    parseLine :: String -> [(Int, Int)]
    parseLine s = zip [0 ..] $map digitToInt s

adjacentLocations :: (Int, Int) -> [(Int, Int)]
adjacentLocations (x, y) =
  [ (x + 1, y),
    (x - 1, y),
    (x, y + 1),
    (x, y - 1)
  ]

-- Given a height map of the basin, find the local minima + their locations
findLowPoints :: Map.Map (Int, Int) Int -> [((Int, Int), Int)]
findLowPoints m =
  Map.foldrWithKey
    ( \key val acc ->
        let adjVals = [x | Just x <- map (`Map.lookup` m) (adjacentLocations key)]
         in let valLowerThanAdjVals = all (val <) adjVals
             in if valLowerThanAdjVals
                  then (key, val) : acc
                  else acc
    )
    []
    m

day9part1 :: Map.Map (Int, Int) Int -> Int
day9part1 m = sum [1 + height | (pos, height) <- findLowPoints m]

data BasinNum
  = Some Int
  | Unassigned
  | Nine
  deriving (Show, Eq)

day9part2 :: Map.Map (Int, Int) Int -> Int
day9part2 m =
  let lowPointLocations = map fst $ findLowPoints m
   in let basinNumberMap =
            Map.map
              ( \case
                  9 -> Nine
                  _ -> Unassigned
              )
              m ::
              Map.Map (Int, Int) BasinNum
       in let seededBasinNumberMap = foldl (\acc (idx, location) -> Map.insert location (Some idx) acc) basinNumberMap (zip [0 ..] lowPointLocations)
           in let basinPoints = transposeBasins $ growBasinsUntilDone seededBasinNumberMap
               in let basinSizes = Map.toList $ Map.map Set.size basinPoints
                   in let sortedBasinSizes = sortBy (\a b -> compare (snd b) (snd a)) basinSizes
                       in product (take 3 (map snd sortedBasinSizes))
  where
    -- turn a map of locations + basin assignments into a map from basin idx to points in the basin
    transposeBasins :: Map.Map (Int, Int) BasinNum -> Map.Map Int (Set.Set (Int, Int))
    transposeBasins basinMap =
      Map.foldlWithKey
        ( \acc key el -> case el of
            Some i -> Map.insertWith Set.union i (Set.singleton key) acc
            _ -> acc
        )
        Map.empty
        basinMap
    growBasinOnce :: Map.Map (Int, Int) BasinNum -> Map.Map (Int, Int) BasinNum
    growBasinOnce basinMap =
      let existingBasins = transposeBasins basinMap
       -- for each basin, add the adjacent points
       in let grownBasins = Map.mapWithKey (\key val -> Set.foldl (\acc el -> Set.union acc $ Set.fromList $ adjacentLocations el) Set.empty val) existingBasins
           -- untranspose the basins, being careful to not claim Nines as being a part of the basin
           in Map.foldlWithKey
                ( \acc basinNumber basinPoints ->
                    foldl
                      ( flip
                          ( Map.adjust
                              ( \case
                                  Unassigned -> Some basinNumber
                                  other -> other
                              )
                          )
                      )
                      acc
                      basinPoints
                )
                basinMap
                grownBasins

    needToGrowBasins :: Map.Map (Int, Int) BasinNum -> Bool
    needToGrowBasins map = Map.foldl (\acc el -> acc || el == Unassigned) False map

    growBasinsUntilDone :: Map.Map (Int, Int) BasinNum -> Map.Map (Int, Int) BasinNum
    growBasinsUntilDone map =
      if needToGrowBasins map
        then growBasinsUntilDone $ growBasinOnce map
        else map

day9main :: IO ()
day9main = do
  handle <- openFile "../inputs2021/day9.txt" ReadMode
  contents <- hGetContents handle
  let aoa = parseToArrayOfArrays contents
  print $ day9part1 aoa
  print $ day9part2 aoa