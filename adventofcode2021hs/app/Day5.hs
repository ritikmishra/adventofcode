module Day5 where

import Control.Monad
import Data.List.Split (splitOn)
import Data.Map hiding (filter, map)
import System.IO
import Text.Read (readMaybe)
import Utils

data Point = Point
  { x :: Int,
    y :: Int
  }
  deriving (Show, Eq, Ord)

data LineSegment = LineSegment {start :: Point, end :: Point} deriving (Show, Eq)

data LineType
  = Vertical
  | Horizontal
  | Diagonal
  | Other
  deriving (Show, Eq)

startX :: LineSegment -> Int
startX = x . start

startY :: LineSegment -> Int
startY = y . start

endX :: LineSegment -> Int
endX = x . end

endY :: LineSegment -> Int
endY = y . end

parsePt :: String -> Maybe Point
parsePt input =
  case splitOn "," input of
    [xStr, yStr] -> do
      xInt <- readMaybe xStr :: Maybe Int
      yInt <- readMaybe yStr :: Maybe Int
      return Point {x = xInt, y = yInt}
    _ -> Nothing

parseLineSegment :: String -> Maybe LineSegment
parseLineSegment input =
  case splitOn " -> " input of
    [start, end] -> do
      startPt <- parsePt start
      endPt <- parsePt end
      return LineSegment {start = startPt, end = endPt}
    _ -> Nothing

lineSegmentToPoints :: LineSegment -> [Point]
lineSegmentToPoints s =
  let smallX = min (startX s) (endX s)
   in let bigX = max (startX s) (endX s)
       in let smallY = min (startY s) (endY s)
           in let bigY = max (startY s) (endY s)
               in case lineType s of
                    Vertical -> map (\y -> Point {x = smallX, y = y}) [smallY .. bigY]
                    Horizontal -> map (\x -> Point {x = x, y = smallY}) [smallX .. bigX]
                    Diagonal ->
                        let xs = if smallX == startX s then [smallX .. bigX] else reverse [smallX .. bigX]
                          in let ys = if smallY == startY s then [smallY .. bigY] else reverse [smallY .. bigY]
                            in zipWith (\x y -> Point {x = x, y = y}) xs ys
                    Other -> error "unsupported"

lineType :: LineSegment -> LineType
lineType seg
  | startX seg == endX seg = Vertical
  | startY seg == endY seg = Horizontal
  | abs (startY seg - endY seg) == abs (startX seg - endX seg) = Diagonal
  | otherwise = Other

isNonDiagonalLine :: LineSegment -> Bool
isNonDiagonalLine seg = Vertical == lineType seg || Horizontal == lineType seg

countPoints :: Map Point Int -> [Point] -> Map Point Int
countPoints state toCount =
    case toCount of
    [] -> state
    x : xs -> countPoints (insertWith (+) x 1 state) xs

day5 :: (LineSegment -> Bool) -> [LineSegment] -> Int
day5 filterStrat lines =
  let straightLines = filter filterStrat lines
   in let points = concatMap lineSegmentToPoints straightLines
       in let pointCounts = countPoints empty points
           in foldrWithKey (\_ count qualified -> if count > 1 then qualified + 1 else qualified) 0 pointCounts

day5main :: IO ()
day5main =
  do
    handle <- openFile "../inputs2021/day5.txt" ReadMode
    contents <- hGetContents handle
    let mbyLineSegs = maybeMap parseLineSegment (lines contents)
    case mbyLineSegs of
      Nothing -> putStrLn "oops!"
      Just l -> do
        putStr "Day 5 part 1: "
        print $ day5 isNonDiagonalLine l
        putStr "Day 5 part 2: "
        print $ day5 ((Other /=) . lineType) l
