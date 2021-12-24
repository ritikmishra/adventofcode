module Day20 where

import Data.Functor (($>))
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Utils (bitsToDecimal, enumerate, maybeMap)

data Image = Image
  { defaultLit :: Bool,
    different :: Map.Map (Int, Int) Bool
  }
  deriving (Show)

getPt :: Image -> (Int, Int) -> Bool
getPt image pt = Maybe.fromMaybe (defaultLit image) (Map.lookup pt (different image))

parseInput :: String -> Maybe ([Bool], Image)
parseInput str = case splitOn "\n\n" str of
  [ruleStr, imageStr] -> do
    let rules = map (== '#') ruleStr
    let points =
          Map.fromList
            [((i, j), c == '#') | (i, l) <- enumerate $ lines imageStr, (j, c) <- enumerate l]
    return (rules, Image {defaultLit = False, different = points})
  _ -> Nothing

pointTonum :: Image -> (Int, Int) -> Int
pointTonum image (i, j) =
  let points = [(i', j') | i' <- [i -1 .. i + 1], j' <- [j -1 .. j + 1]]
   in let isMember = map (getPt image) points
       in bitsToDecimal isMember

setBounds :: Map.Map (Int, Int) Bool -> ((Int, Int), (Int, Int))
setBounds m = 
    let ((minR, minC), (maxR, maxC)) = (minimum $ Map.keys m, maximum $ Map.keys m)
    in ((minR, maxR), (minC, maxC))

imgBounds :: Image -> ((Int, Int), (Int, Int))
imgBounds = setBounds . different

convolveOnce :: [Bool] -> Image -> Image
convolveOnce rules img =
  let ((minI, maxI), (minJ, maxJ)) = imgBounds img
   in let pts = [(i, j) | i <- [minI - 2 .. maxI + 2], j <- [minJ - 2 .. maxJ + 2]]
       in let newpts = foldl (\acc pt -> Map.insert pt (rules !! pointTonum img pt) acc) Map.empty pts
           in -- 0 goes to 1, off by default        -- 1 goes to 0, on by default
              if head rules && not (defaultLit img) || not (last rules) && defaultLit img
                then
                  Image
                    { defaultLit = not $ defaultLit img,
                      different = newpts
                    }
                else
                  img
                    { different = newpts
                    }

printImg :: Image -> String
printImg img =
  let ((minI, maxI), (minJ, maxJ)) = imgBounds img
   in let pts = [[if getPt img (i, j) then '#' else '.' | j <- [minJ - 2 .. maxJ + 2]] | i <- [minI - 2 .. maxI + 2]]
       in foldl1 (\acc el -> acc ++ '\n' : el) pts

count :: Image -> Int
count img = 
    Map.foldr (\el acc -> acc + if el then 1 else 0) 0 (different img)

day20main :: IO ()
day20main = do
  handle <- openFile "../inputs2021/day20.txt" ReadMode
  contents <- hGetContents handle
  case parseInput contents of
    Nothing -> putStrLn "could not parse input"
    Just (rules, img) -> do
      let c = convolveOnce rules
      print $ imgBounds img
      putStrLn "day 20 part 1:"
      print $ count (c $ c img)
      putStrLn "day 20 part 2: "
      print $ count (foldl (\acc el ->  c acc) img [1..50])