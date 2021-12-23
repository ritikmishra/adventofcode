module Day19 where

import qualified Data.Bifunctor
import Data.Foldable (maximumBy)
import Data.List (sort)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Utils (enumerate)
import VectorMath (Addable (add), Mat3, Multipliable (mul), Subtractable (sub), Vec3 (Vec3), all3DRotations, magnitude)

newtype Scanner = Scanner [Vec3] deriving (Show, Eq)

parseScanners :: String -> [Scanner]
parseScanners inp =
  let scannerTexts = splitOn "\n\n" inp
   in map parseSingleScanner scannerTexts
  where
    parseVec :: String -> Vec3
    parseVec s =
      case splitOn "," s of
        [a, b, c] -> Vec3 (read a, read b, read c)
        _ -> error "attempted to parse a vec3 that did not have 3 elements"
    parseSingleScanner :: String -> Scanner
    parseSingleScanner s = Scanner $ map parseVec (tail (lines s))

translate :: Vec3 -> [Vec3] -> [Vec3]
translate t = map (`add` t)

checkAlignmentBetween2Scanners :: Scanner -> Scanner -> (Vec3, Int)
checkAlignmentBetween2Scanners (Scanner sA) (Scanner sB) =
  let deltas =
        concatMap
          ( \(i, ai) ->
              map
                ( \(j, bj) ->
                    ai `sub` bj
                )
                $ enumerate sB
          )
          $ enumerate sA
   in let freqs = foldl (\acc el -> Map.insertWith (+) el 1 acc) Map.empty deltas
       in Map.foldrWithKey
            (\key val a@(maxKey, maxVal) -> if val > maxVal then (key, val) else a)
            (Vec3 (0, 0, 0), -1) -- map should only contain positive values
            freqs

-- findBestAlignment :: Scanner -> Scanner -> (Mat3, Vec3, Int)
findBestAlignment :: Scanner -> Scanner -> (Scanner, Vec3, Int)
findBestAlignment a@(Scanner sA) (Scanner sB) =
  let rotated_sB's = map (\r -> Scanner $ map (r `mul`) sB) all3DRotations
   in let (Scanner r, (t, count)) = maximumBy (\a b -> compare (snd $ snd a) (snd $ snd b)) $ map (\b -> (b, checkAlignmentBetween2Scanners a b)) rotated_sB's
       in (Scanner $ translate t r, t, count)

removeIth :: Int -> [a] -> [a]
removeIth n l = map snd $ filter (\(i, _) -> i /= n) $ enumerate l

combineScanners :: [Scanner] -> (Scanner, [Vec3])
combineScanners [] = error "no scanners?"
combineScanners [a] = (a, [Vec3 (0, 0, 0)])
combineScanners (a@(Scanner aPoints) : bs) =
  let translatedScanners = map (Data.Bifunctor.second (findBestAlignment a)) $ enumerate bs
   in let (i, (Scanner bestAlignedB, t, _)) = maximumBy (\a' b' -> compare (last a') (last b')) translatedScanners
       in let combined = Scanner $ Set.toList $ Set.union (Set.fromList aPoints) (Set.fromList bestAlignedB)
           in let (combined', ts) = combineScanners (combined : removeIth i bs)
               in (combined', t : ts)
  where
    last (_, (_, _, a)) = a

manhattan :: Vec3 -> Vec3 -> Int
manhattan (Vec3 (a1, a2, a3)) (Vec3 (b1, b2, b3)) =
  abs (a1 - b1) + abs (a2 - b2) + abs (a3 - b3)

day19main :: IO ()
day19main = do
  handle <- openFile "../inputs2021/day19.txt" ReadMode
  contents <- hGetContents handle
  let scanners = parseScanners contents
  -- print $ findBestAlignment (head scanners) $ scanners !! 1
  let (Scanner x, ts) = combineScanners scanners
  print $ length x
  -- print $ sort x
  print $ maximum [manhattan a b | a <- ts, b <- ts]
