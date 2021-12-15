module Day13 where

import Data.Foldable (maximumBy)
import Data.List.Split (splitOn)
import qualified Data.Set as Set
import System.IO
import Text.Read (readMaybe)
import Utils (maybeMap, splitAtNextCharacter)

newtype Dots = Dots (Set.Set (Int, Int)) deriving (Show, Eq)

data FoldInstruction = X Int | Y Int deriving (Show, Eq)

parseInput :: String -> Maybe (Dots, [FoldInstruction])
parseInput inp =
  case splitOn "\n\n" inp of
    [dotsStr, foldStr] -> do
      dots <- parseDot dotsStr
      folds <- parseFolds foldStr
      return (dots, folds)
    _ -> Nothing
  where
    parseDot :: String -> Maybe Dots
    parseDot dotLines =
      let dotStrs = lines dotLines
       in let maybeDotList =
                maybeMap
                  ( \dot ->
                      let (xStr, yStr) = splitAtNextCharacter ',' dot
                       in do
                            x <- readMaybe xStr :: Maybe Int
                            y <- readMaybe yStr :: Maybe Int
                            return (x, y)
                  )
                  dotStrs
           in do
                Dots . Set.fromList <$> maybeDotList

    parseFolds :: String -> Maybe [FoldInstruction]
    parseFolds foldLines =
      maybeMap
        ( ( \foldInstr ->
              let (kind, numStr) = splitAtNextCharacter '=' foldInstr
               in do
                    num <- readMaybe numStr :: Maybe Int
                    case kind of
                      "x" -> return $ X num
                      "y" -> return $ Y num
                      _ -> Nothing
          )
            . drop 11
        )
        (lines foldLines)

doAFold :: Dots -> FoldInstruction -> Dots
doAFold (Dots dots) (X xEquals) =
  Dots $
    Set.map
      ( \pt@(x, y) ->
          if x >= xEquals
            then (2 * xEquals - x, y)
            else pt
      )
      dots
doAFold (Dots dots) (Y yEquals) =
  Dots $
    Set.map
      ( \pt@(x, y) ->
          if y >= yEquals
            then (x, 2 * yEquals - y)
            else pt
      )
      dots

dotsSize (Dots d) = Set.size d

printDots :: Dots -> String
printDots (Dots dset) =
  let (maxX, maxY) = maximum dset
    in let lines = [[if Set.member (x, y) dset then '▉' else ' ' | x <- [0..maxX]] | y <- [0..maxY + 1]]
    in foldl1 (\acc el -> acc ++ '\n':el) lines

day13main :: IO ()
day13main = do
  handle <- openFile "../inputs2021/day13.txt" ReadMode
  contents <- hGetContents handle
  case parseInput contents of
    Nothing -> print "Could not parse input"
    Just (dots, foldInstructions) -> do
      putStrLn "Day 12 part 1: "
      print $ dotsSize $ doAFold dots (head foldInstructions)
      putStrLn "Day 12 part 2: "
      putStrLn $ printDots $ foldl doAFold dots foldInstructions
