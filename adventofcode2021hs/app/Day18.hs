module Day18 where

import Control.Monad.State
import Data.Char (digitToInt, isDigit)
import Debug.Trace (trace)
import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Utils (maybeMap)

data FishNum = Val Int | Pair (FishNum, FishNum) deriving (Show, Eq)

data StackElement
  = OpeningBrace
  | Num FishNum
  deriving (Show, Eq)

parseFishNum :: [StackElement] -> String -> Maybe FishNum
parseFishNum acc stringedNum =
  case stringedNum of
    [] -> case acc of
      [Num num] -> Just num
      _ -> Nothing
    '[' : xs -> parseFishNum (OpeningBrace : acc) xs
    ',' : xs -> parseFishNum acc xs -- ignore commas
    ']' : xs -> do
      (first, rest) <- safeHead acc
      (second, rest') <- safeHead rest
      (third, rest'') <- safeHead rest'
      case (first, second, third) of
        (Num num2, Num num1, OpeningBrace) -> parseFishNum (Num (Pair (num1, num2)) : rest'') xs
        _ -> Nothing
    digit : xs ->
      if isDigit digit
        then parseFishNum (Num (Val (digitToInt digit)) : acc) xs
        else Nothing
  where
    safeHead :: [a] -> Maybe (a, [a])
    safeHead [] = Nothing
    safeHead (x : xs) = Just (x, xs)

data ExplosionState
  = Depth Int
  | Exploded
  deriving (Show, Eq)

printPair :: FishNum -> String
printPair (Val n) = show n
printPair (Pair (l, r)) = "[" ++ printPair l ++ "," ++ printPair r ++ "]"

increment (Depth d) = Depth $ d + 1
increment Exploded = Exploded

leftAdd :: FishNum -> Int -> FishNum
leftAdd x 0 = x
leftAdd (Val x) y = Val (x + y)
leftAdd (Pair (x1, x2)) y = Pair (leftAdd x1 y, x2)

rightAdd :: FishNum -> Int -> FishNum
rightAdd x 0 = x
rightAdd (Val x) y = Val (x + y)
rightAdd (Pair (x1, x2)) y = Pair (x1, rightAdd x2 y)

explodeFishNum :: Int -> FishNum -> (FishNum, Maybe (Int, Int))
explodeFishNum s num =
  let couldExplode = s == 4
   in let s' = s + 1
       in case num of
            -- Explode the pair, and record the left and right deltas
            Pair (Val l, Val r) | couldExplode -> (Val 0, Just (l, r))
            -- Single values cannot be exploded
            Val n -> (num, Nothing)
            -- A pair of 2 items where we cannot explode
            Pair (l, r) -> case explodeFishNum s' l of
              (l', Just (dl, dr)) -> (Pair (l', leftAdd r dr), Just (dl, 0))
              (l', Nothing) -> case explodeFishNum s' r of
                (r', Just (dl, dr)) -> (Pair (rightAdd l' dl, r'), Just (0, dr))
                (r', Nothing) -> (Pair (l', r'), Nothing)

splitFishNum :: FishNum -> FishNum
splitFishNum f@(Val n)
  | n < 10 = f
  | otherwise = Pair (Val (n `div` 2), Val (n `div` 2 + n `mod` 2))
splitFishNum (Pair (left, right)) =
  let left' = splitFishNum left
   in if left' /= left
        then Pair (left', right)
        else Pair (left, splitFishNum right)

reduce :: FishNum -> FishNum
reduce n =
  let n' = reduceOnce n
   in if n' /= n
        then reduce n'
        else n'
  where
    reduceOnce n =
      let (exploded, _) = explodeFishNum 0 n
       in if n /= exploded
            then exploded
            else
              let split = splitFishNum exploded
               in split

-- reduceOnce = splitFishNum . fst . explodeFishNum (Depth 0)

addFishNums :: FishNum -> FishNum -> FishNum
addFishNums a b = reduce $ Pair (a, b)

magnitude :: FishNum -> Int
magnitude (Val n) = n
magnitude (Pair (l, r)) = 3 * magnitude l + 2 * magnitude r

day18main :: IO ()
day18main = do
  handle <- openFile "../inputs2021/day18.txt" ReadMode
  contents <- hGetContents handle
  case maybeMap (parseFishNum []) $ lines contents of
    Just xs -> do
        putStr "Day 1 part 1: "
        print $ magnitude $ foldl1 addFishNums xs
        putStr "Day 1 part 2: "
        print $ maximum $ [magnitude $ addFishNums a b | a <- xs, b <- xs]
    _ -> putStrLn "error parsing fishnums"
