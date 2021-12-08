module Day8 where

import Control.Monad (join)
import Data.List (find, sort)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Map ((!))
import qualified Data.Semigroup as Set
import qualified Data.Set as Set
import Debug.Trace
import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Utils (maybeMap)

data Display = A | B | C | D | E | F | G deriving (Show, Eq, Ord)

newtype Digit = Digit [Display] deriving (Show, Eq)

-- Read character from the input into the Display data type
-- Fallible (may return nothing if the character is not a-g)
readDisplay :: Char -> Maybe Display
readDisplay 'a' = Just A
readDisplay 'b' = Just B
readDisplay 'c' = Just C
readDisplay 'd' = Just D
readDisplay 'e' = Just E
readDisplay 'f' = Just F
readDisplay 'g' = Just G
readDisplay _ = Nothing

-- Data holder for line from the input
-- signal pattern comes from left of | symbol
-- output value comes from right
data Line = Line {signalPattern :: [Digit], outputValue :: [Digit]} deriving (Show, Eq)

-- Parse 1 line of input into a line (fallible)
parseLine :: String -> Maybe Line
parseLine str =
  case splitOn " | " str of
    [sigPatText, outValText] -> do
      sigPat <- maybeMap parseStringToDisplayString $ splitOn " " sigPatText
      outVal <- maybeMap parseStringToDisplayString $ splitOn " " outValText
      return
        Line
          { signalPattern = map Digit sigPat,
            outputValue = map Digit outVal
          }
    _ -> Nothing
  where
    parseStringToDisplayString :: String -> Maybe [Display]
    parseStringToDisplayString str = maybeMap readDisplay str

parseDay8Text :: String -> Maybe [Line]
parseDay8Text = maybeMap parseLine . lines

-- Count the number of 1's, 4's, 7's, 8's in the input lines
day8part1 :: [Line] -> Int
day8part1 lines =
  let outputVals = map outputValue lines
   in let flattened = join outputVals
       in sum
            [ case length digit of
                2 -> 1 -- digit 1
                4 -> 1 -- digit 4
                3 -> 1 -- digit 7
                7 -> 1 -- digit 8
                _ -> 0
              | (Digit digit) <- flattened
            ]

-- full set of display bits
fullSet = Set.fromList [A, B, C, D, E, F, G]

initMap :: Map.Map Display (Set.Set Display)
initMap = foldl (\acc el -> Map.insert el fullSet acc) Map.empty [A, B, C, D, E, F, G]

-- Narrow down possibilities based on the fact that 1's, 4's, 7's are unambiguously identifiable
deduce1 :: Map.Map Display (Set.Set Display) -> [Digit] -> Map.Map Display (Set.Set Display)
deduce1 acc [] = acc
deduce1 acc (Digit x : xs) =
  let set = do
        (groundTruthB, groundTruth) <- case length x of
          -- Digit 1
          2 -> Just (Set.fromList x, [C, F])
          -- Digit 4
          4 -> Just (Set.fromList x, [B, C, D, F])
          -- Digit 7
          3 -> Just (Set.fromList x, [A, C, F])
          _ -> Nothing
        let groundTruthSet = Set.fromList groundTruth
        let acc' =
              Map.mapWithKey
                ( \key val ->
                    if Set.member key groundTruthB
                      then -- If this key is in the number, it may map to something in the ground truth set
                        Set.intersection val groundTruthSet
                      else -- If this key is not in the number, it cannot possibly map to something in the ground truth set
                        Set.difference val groundTruthSet
                )
                acc
        return acc'
   in case set of
        Just acc' -> deduce1 acc' xs
        Nothing -> deduce1 acc xs

-- Narrow down possibilities based on the fact that 0's, 6's, and 9's are only missing one 
-- of the bits
deduce2 :: Map.Map Display (Set.Set Display) -> [Digit] -> Map.Map Display (Set.Set Display)
deduce2 acc [] = acc
deduce2 acc (Digit x : xs)
  | length x == 6 =
    case Set.toList (Set.difference fullSet $ Set.fromList x) of
      [x] ->
        let acc' = Map.insertWith Set.intersection x (Set.fromList [E, D, C]) acc
         in deduce2 acc' xs
      _ -> error "wat"
  | otherwise = deduce2 acc xs

-- Turn a 1 element set into its 1 element
extractFromSet :: Set.Set p -> p
extractFromSet s = case Set.toList s of
  [x] -> x
  _ -> error "could not extract single element from list"

-- Narrow down the possibilities based on the fact that 
deduce3 :: Map.Map Display (Set.Set Display) -> Map.Map Display Display
deduce3 acc =
  let solvedCharacters =
        Map.foldlWithKey
          ( \acc key val ->
              if Set.size val == 1
                then Set.union acc val
                else acc
          )
          Set.empty
          acc
   in let singletonSets = Map.map (\s -> if Set.size s == 1 then s else Set.difference s solvedCharacters) acc
       in Map.map extractFromSet singletonSets

deduce :: [Digit] -> Map.Map Display Display
deduce digits =
  let a1 = deduce1 initMap digits
   in (deduce3 . (deduce2 . deduce1 initMap) digits) digits

-- Use the conversion table to map a digit to the actual digit
mapDigit :: Map.Map Display Display -> Digit -> Digit
mapDigit conversionTable (Digit bad) = Digit (map (conversionTable!) bad)

digitToNumber :: Digit -> Maybe Int
digitToNumber (Digit segments) =
  case sort segments of
    [A, B, C, E, F, G] -> Just 0
    [C, F] -> Just 1
    [A, C, D, E, G] -> Just 2
    [A, C, D, F, G] -> Just 3
    [B, C, D, F] -> Just 4
    [A, B, D, F, G] -> Just 5
    [A, B, D, E, F, G] -> Just 6
    [A, C, F] -> Just 7
    [A, B, C, D, E, F, G] -> Just 8
    [A, B, C, D, F, G] -> Just 9
    _ -> Nothing

digitStrToNumber :: (Digit -> Maybe Int) -> [Digit] -> Maybe Int
digitStrToNumber _ [] = Just 0
digitStrToNumber converter ds =
    let x = zip (reverse ds) [0..]
        in foldl (\acc (el, power) -> do
                                e <- acc
                                num <- converter el
                                return $ (10^power) * num + e
            ) (Just 0) x

day8part2 :: [Line] -> Maybe Int
day8part2 lines = 
    do 
        outputValues <- maybeMap solveLine lines
        return $ sum outputValues
    where
        solveLine :: Line -> Maybe Int
        solveLine line = 
            let conversionTable = (deduce . signalPattern) line
              in digitStrToNumber (digitToNumber . mapDigit conversionTable) (outputValue line)

day8main :: IO ()
day8main = do
  handle <- openFile "../inputs2021/day8.txt" ReadMode
  contents <- hGetContents handle
  case parseDay8Text contents of
    Nothing -> print "could not parse :("
    Just lines -> do
      putStr "Day 8 Part 1: "
      print $ day8part1 lines
      putStr "Day 8 Part 2: "
      print $ day8part2 lines
