module Day22 where

import Data.Foldable (find)
import Data.List (partition)
import Data.List.Split (splitOn)
import qualified Data.Set as Set
import Debug.Trace (trace)
import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Text.Read (readMaybe)
import Utils (maybeMap, splitAtNextCharacter)

type Point = (Int, Int, Int)

type Cube = (Point, Point)

cubeCorners :: Cube -> [Point]
cubeCorners ((minX, minY, minZ), (maxX, maxY, maxZ)) = [(x, y, z) | x <- [minX, maxX -1], y <- [minY, maxY -1], z <- [minZ, maxZ -1]]

pointInCube :: Cube -> Point -> Bool
pointInCube ((minX, minY, minZ), (maxX, maxY, maxZ)) (x, y, z) =
  minX <= x && x < maxX
    && minY <= y
    && y < maxY
    && minZ <= z
    && z < maxZ

data CubeIntersection
  = WhollyContained [Point]
  | Hole [Point]
  | Corner Point

cubesIntersect :: Cube -> Cube -> Maybe CubeIntersection
cubesIntersect a b =
  case filter (pointInCube a) (cubeCorners b) of
    [] -> Nothing
    [x] -> Just $ Corner x
    ps@[a, b, c, d] -> Just $ Hole ps
    ps@[a, b, c, d, e, f, g, h] -> Just $ WhollyContained ps
    _ -> error "what cube intersection?"

data Cut = X Int | Y Int | Z Int

cutCube :: Cut -> Cube -> [Cube]
cutCube (X cut) orig@(a@(minX, minY, minZ), b@(maxX, maxY, maxZ))
  | minX < cut && cut < maxX =
    [ (a, (cut, maxY, maxZ)),
      ((cut, minY, minZ), b)
    ]
  | otherwise = [orig]
cutCube (Y cut) orig@(a@(minX, minY, minZ), b@(maxX, maxY, maxZ))
  | minY < cut && cut < maxY =
    [ (a, (maxX, cut, maxZ)),
      ((minX, cut, minZ), b)
    ]
  | otherwise = [orig]
cutCube (Z cut) orig@(a@(minX, minY, minZ), b@(maxX, maxY, maxZ))
  | minZ < cut && cut < maxZ =
    [ (a, (maxX, maxY, cut)),
      ((minX, minY, cut), b)
    ]
  | otherwise = [orig]

cubeVolume :: Cube -> Int
cubeVolume ((minX, minY, minZ), (maxX, maxY, maxZ)) =
  let dx = maxX - minX
   in let dy = maxY - minY
       in let dz = maxZ - minZ
           in dx * dy * dz

pointToCuts :: Point -> [Cut]
pointToCuts (x, y, z) = [X x, Y y, Z z]

splitCubeAtPoint :: Cube -> Point -> [Cube]
splitCubeAtPoint cube c@(x, y, z) =
  let cuts = [X x, Y y, Z z]
   in foldl (\cubes cut -> concatMap (cutCube cut) cubes) [cube] cuts

applyCuts :: [Cube] -> [Cut] -> [Cube]
applyCuts = foldl (\cubes cut -> concatMap (cutCube cut) cubes)

safeDifference :: Cube -> Cube -> [Cube]
safeDifference a b@(b1, b2) =
  let cuts = pointToCuts b1 ++ pointToCuts b2
   in let aSplit = applyCuts [a] cuts
       in filter (not . any (pointInCube b) . cubeCorners) aSplit

parseInstrs :: String -> [(Bool, Cube)]
parseInstrs = map parseLine . lines
  where
    parseLine :: String -> (Bool, Cube)
    parseLine s =
      let (onOrOffs, xyzs) = splitAtNextCharacter ' ' s
       in let x = map (map read . splitOn ".." . snd . splitAtNextCharacter '=') (splitOn "," xyzs) :: [[Int]]
           in case (onOrOffs, x) of
                ("on", [[x1, x2], [y1, y2], [z1, z2]]) -> (True, ((x1, y1, z1), (x2 + 1, y2 + 1, z2 + 1)))
                ("off", [[x1, x2], [y1, y2], [z1, z2]]) -> (False, ((x1, y1, z1), (x2 + 1, y2 + 1, z2 + 1)))
                _ -> error "error parsing line"

executeInstrs :: [(Bool, Cube)] -> [Cube]
executeInstrs [] = []
executeInstrs ((_, firstCube) : instrs) =
  foldl
    ( \acc (shouldUnion, cube) ->
        let existingCubesWithoutNewOne = concatMap (`safeDifference` cube) acc
         in if shouldUnion
              then cube : existingCubesWithoutNewOne
              else existingCubesWithoutNewOne
    )
    [firstCube]
    instrs

day22main :: IO ()
day22main = do
  handle <- openFile "../inputs2021/day22.txt" ReadMode
  contents <- hGetContents handle
  let instrs = parseInstrs contents
  putStrLn "Day 22 part 1:"
  print $ sum $ map cubeVolume $ executeInstrs $ take 10 instrs
  putStrLn "Day 22 part 2:"
  print $ sum $ map cubeVolume $ executeInstrs instrs
