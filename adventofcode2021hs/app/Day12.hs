module Day12 where

import Data.Char (isUpper)
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Utils (splitAtNextCharacter)

-- Adjacency list representation of a graph
newtype Graph = Graph (Map.Map String (Set.Set String)) deriving (Eq, Show)

parseDay12Text :: String -> Graph
parseDay12Text caveLines =
  let pairs = map parseSingleLine $ lines caveLines
   in Graph $
        foldl
          ( \acc (start, end) ->
              -- Connect start->end as well as end->start
              let acc' = Map.insertWith Set.union start (Set.singleton end) acc
               in Map.insertWith Set.union end (Set.singleton start) acc'
          )
          Map.empty
          pairs
  where
    parseSingleLine :: String -> (String, String)
    parseSingleLine line = splitAtNextCharacter '-' line

newtype Path = Path [String] deriving (Eq, Show)

isBigCave :: String -> Bool
isBigCave s = isUpper $ s !! 0

newtype VisitPredicate = VisitPredicate
  { -- Try to visit a node
    -- - If we cannot visit the node, return None
    -- - If we can visit the node, return a new VisitPredicate accounting for the fact we visited this node
    onVisitingNode :: String -> Maybe VisitPredicate
  }

findPaths :: String -> String -> VisitPredicate -> Graph -> [Path]
findPaths start end visited g@(Graph graph) =
  if start == end
    then [Path [end]] -- If we hit the end, then our path is immediately over
    else case onVisitingNode visited start of
      Nothing -> [] -- Nothing -> we are not allowed to visit this node
      Just visited' ->
        -- Try to visit all of the other paths
        let possibleNextLocations = Maybe.fromMaybe Set.empty (Map.lookup start graph)
         in let paths = concatMap (\loc -> findPaths loc end visited' g) possibleNextLocations
             in -- Remember that we visited this node
                map (\(Path p) -> Path (start : p)) paths

visitPredicate1 = makeVisitPredicatePt1' Set.empty :: VisitPredicate
  where
    makeVisitPredicatePt1' nodesVisited =
      VisitPredicate
        { onVisitingNode = \s ->
            if Set.member s nodesVisited
              then Nothing
              else
                let nodesVisited' =
                      if isBigCave s
                        then nodesVisited
                        else Set.insert s nodesVisited
                 in Just $ makeVisitPredicatePt1' nodesVisited'
        }

visitPredicate2 = makeVisitPredicatePt2' visitPredicate1 Nothing :: VisitPredicate
  where
    makeVisitPredicatePt2' :: VisitPredicate -> Maybe String -> VisitPredicate
    makeVisitPredicatePt2' delegate Nothing =
      VisitPredicate
        { onVisitingNode = \s ->
            case onVisitingNode delegate s of
              Just delegate' -> Just $ makeVisitPredicatePt2' delegate' Nothing
              -- hardcoded that we can only visit start once
              Nothing -> if s /= "start" then Just $ makeVisitPredicatePt2' delegate (Just s) else Nothing
        }
    makeVisitPredicatePt2' delegate (Just _) = delegate

day12part1 = length . findPaths "start" "end" visitPredicate1
day12part2 = length . findPaths "start" "end" visitPredicate2

day12main :: IO ()
day12main = do
  handle <- openFile "../inputs2021/day12.txt" ReadMode
  contents <- hGetContents handle
  let parsed = parseDay12Text contents
  putStrLn "Day 12 part 1: "
  print $ day12part1 parsed
  putStrLn "Day 12 part 2: "
  print $ day12part2 parsed
