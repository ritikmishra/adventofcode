module Day15 where

import Data.Char (digitToInt)
import Data.Foldable (minimumBy)
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace (trace)
import System.IO

parseGrid :: String -> Map.Map (Int, Int) Int
parseGrid grid =
  Map.fromList $
    concatMap
      (\(rowNum, line) -> zipWith (\colNum character -> ((rowNum, colNum), digitToInt character)) [0 ..] line)
      (zip [0 ..] (lines grid))

cycleNine n = if n <= 9 then n else cycleNine (n - 9)

-- makeMapFiveTimesLarger :: Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
makeMapFiveTimesLarger map =
  let (maxX, maxY) = maximum $ Map.keys map
   in let (newMaxX, newMaxY) = ((maxX + 1) * 5 - 1, (maxY + 1) * 5 - 1)
       in Map.fromList $
            [ 
              ( \c@(x, y) ->
                  let (xTileNum, yTileNum) = (x `div` (maxX + 1), y `div` (maxY + 1))
                  in let cc = (x `mod` (maxX + 1), y `mod` (maxY + 1))
                   in (c, cycleNine (map ! cc + xTileNum + yTileNum))
              )
               (x, y)
              | x <- [0 .. newMaxX],
                y <- [0 .. newMaxY]
            ]

data Edge = Edge
  { otherNode :: (Int, Int),
    cost :: Int
  }
  deriving (Show, Eq, Ord)

newtype AdjList = AdjList (Map.Map (Int, Int) (Set.Set Edge)) deriving (Eq, Show)

adjacentLocations :: (Int, Int) -> [(Int, Int)]
adjacentLocations (x, y) =
  [ (x + 1, y),
    (x - 1, y),
    (x, y + 1),
    (x, y - 1)
  ]

gridToAdjList :: Map.Map (Int, Int) Int -> AdjList
gridToAdjList grid =
  AdjList $
    Map.foldrWithKey
      ( \key val acc ->
          let edges = [Edge {otherNode = neighbor, cost = cost} | neighbor <- adjacentLocations key, Just cost <- [Map.lookup neighbor grid]]
           in foldl (\acc' edge -> Map.insertWith Set.union key (Set.singleton edge) acc') acc edges
      )
      Map.empty
      grid

extractMinFromSet :: Ord a => Set.Set a -> (Set.Set a, a)
extractMinFromSet a =
  let smolest = minimum a
   in (Set.delete smolest a, smolest)

-- dijkstra :: AdjList -> Map.Map (Int, Int) (Maybe (Int, Int))
dijkstra a@(AdjList list) =
  let pi = Map.map (const Nothing) list
   in let d = Map.insert (0, 0) 0 $ Map.map (const 1000000000) list
       in foo a ((Set.empty), d, pi)

type PredecessorMap = (Map.Map (Int, Int) (Maybe (Int, Int)))

type Visited = Set.Set (Int, Int)

type CostMap = (Map.Map (Int, Int) Int)

type LoopState = (Visited, CostMap, PredecessorMap)

foo :: AdjList -> LoopState -> LoopState
foo a@(AdjList adjList) s@(visited, d, pi) =
  if length visited == length adjList
    then s
    else
      let unvisitedNodes = filter (`Set.notMember` visited) (Map.keys adjList)
       in let nearestUnvisitedNode = minimumBy (\a b -> compare (d ! a) (d ! b)) unvisitedNodes
           in let x = adjList ! nearestUnvisitedNode
               in let (d', pi') =
                        foldl
                          ( \o@(d', pi') el ->
                              if cost el + d ! nearestUnvisitedNode < d ! otherNode el
                                then (Map.insert (otherNode el) (cost el + d ! nearestUnvisitedNode) d', Map.insert (otherNode el) (Just nearestUnvisitedNode) pi')
                                else o
                          )
                          (d, pi)
                          x
                   in foo a (Set.insert nearestUnvisitedNode visited, d', pi')

getPath :: (Int, Int) -> PredecessorMap -> [(Int, Int)]
getPath dest pi =
  if dest == (0, 0)
    then []
    else case pi ! dest of
      Nothing -> error "bruh what"
      Just destPred -> getPath destPred pi ++ [dest]

-- let smolest =

day15main :: IO ()
day15main = do
  handle <- openFile "../inputs2021/day15.txt" ReadMode
  contents <- hGetContents handle
  let origGrid = parseGrid contents
  let bigGrid = makeMapFiveTimesLarger origGrid

  let adjList = gridToAdjList bigGrid
  let (visited, costMap, predecessorMap) = dijkstra adjList
  print $ costMap ! (499, 499)

--   let bigAdjList = gridToAdjList bigGrid
--   let
