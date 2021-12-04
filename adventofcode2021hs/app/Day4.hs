{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day4 where

import Data.Char (isSpace)
import Data.List
import Data.List.Split (splitOn)
import Debug.Trace
import System.IO
import Text.Read
import Utils

data BingoNumber
  = Unmarked Int
  | Marked Int
  deriving (Show, Eq)

newtype BingoBoard = BingoBoard [[BingoNumber]] deriving (Show)

newtype BingoNumberOrder = BingoNumberOrder [Int] deriving (Show)

-- Parse a comma separated list of numbers into a BingoNumberOrder
parseNumbers :: String -> Maybe BingoNumberOrder
parseNumbers string = do
  nums <- maybeMap readMaybe $ splitAllCharacters ',' string
  return $ BingoNumberOrder nums

-- Parse a multi-line string representing a bingo board intto a BingoBoard
parseBoard :: String -> Maybe BingoBoard
parseBoard boardString =
  let boardRows = splitOn "\n" boardString :: [String]
   in do
        boardNums <- maybeMap parseRow boardRows
        return $ BingoBoard $ map (map Unmarked) boardNums
  where
    parseRow :: String -> Maybe [Int]
    parseRow = maybeMap readMaybe . filter (/= "") . splitOn " "

-- Parse the input text into the BingoNumberOrder and the [BingoBoard]
parseText :: String -> Maybe (BingoNumberOrder, [BingoBoard])
parseText text =
  case splitOn "\n\n" text of
    [] -> Nothing
    numberStrs : boards -> do
      nums <- parseNumbers numberStrs
      boards <- maybeMap parseBoard boards
      return (nums, boards)

-- On a BingoBoard, convert all Unmarked bingo numbers that match the number 
-- that has been called out into Marked ones
markNumberOnBingoBoard :: Int -> BingoBoard -> BingoBoard
markNumberOnBingoBoard numToMark (BingoBoard board) = BingoBoard $ map (replaceNumInRow numToMark) board
  where
    replaceNumInRow :: Int -> [BingoNumber] -> [BingoNumber]
    replaceNumInRow n =
      map
        ( \case
            Unmarked i -> if i == n then Marked i else Unmarked i
            Marked i -> Marked i
        )

-- Check if a BingoBoard has a complete row/column of Marked entries
isBingoBoardAWinner :: BingoBoard -> Bool
isBingoBoardAWinner (BingoBoard board) =
  let isMarked =
        map
          ( map
              ( \case
                  Marked _ -> True
                  Unmarked _ -> False
              )
          )
          board
   in let thereIsACompletelyMarkedRow = any ((== True) . all (== True)) isMarked
       in let thereIsACompletelyMarkedColumn = any ((== True) . all (== True)) (transpose isMarked)
           in thereIsACompletelyMarkedRow || thereIsACompletelyMarkedColumn

-- Continue marking boards as per the call out order until we have a winner
-- Return the bingo board that won + the last number that was called out
markBoardsUntilWinner :: BingoNumberOrder -> [BingoBoard] -> Maybe (BingoBoard, Int)
markBoardsUntilWinner (BingoNumberOrder order) boards =
  case order of
    [] -> Nothing
    n : ns ->
      let markedBoards = map (markNumberOnBingoBoard n) boards
       in case find isBingoBoardAWinner markedBoards of
            Nothing -> markBoardsUntilWinner (BingoNumberOrder ns) markedBoards
            Just winner -> Just (winner, n)

-- Find the last bingo board(s) to win
-- Return the bingo boards as well as the last number that was called out
findLastWinner :: BingoNumberOrder -> [BingoBoard] -> Maybe ([BingoBoard], Int)
findLastWinner (BingoNumberOrder order) boards =
  case order of
    [] -> Nothing
    n : ns ->
      let markedBoards = map (markNumberOnBingoBoard n) boards
       in case markedBoards of
           [] -> Nothing 
           manyBoards -> 
               if all isBingoBoardAWinner manyBoards then
                   Just (manyBoards, n)
                else
                    findLastWinner (BingoNumberOrder ns) (filter (not . isBingoBoardAWinner) manyBoards)

-- Calculate the score of a marked board
calcBoardScore :: Int -> BingoBoard -> Int
calcBoardScore lastCalledNum (BingoBoard board) =
  let preMult =
        ( sum
            . map
              ( \case
                  Unmarked i -> i
                  Marked _ -> 0
              )
            . concat
        )
          board
   in preMult * lastCalledNum

day4part1 :: String -> Maybe Int
day4part1 x =
  do
    (numberOrder, boards) <- parseText x
    (winningBoard, lastCalledNumber) <- markBoardsUntilWinner numberOrder boards
    return $ calcBoardScore lastCalledNumber winningBoard

day4part2 :: String -> Maybe [Int]
day4part2 x =
  do
    (numberOrder, boards) <- parseText x
    (winningBoards, lastCalledNumber) <- findLastWinner numberOrder boards
    return $ map (calcBoardScore lastCalledNumber) winningBoards

day4main :: IO ()
day4main =
  do
    handle <- openFile "../inputs2021/day4.txt" ReadMode
    contents <- hGetContents handle
    case doBothParts contents of
      Nothing -> do
        putStrLn "could not parse data"
      Just (part1ans, part2ans) -> do
        putStr "Day 4 Part 1 answer: "
        print part1ans
        putStr "Day 4 Part 2 answer: "
        print part2ans
        

  where 
      doBothParts :: String -> Maybe (Int, [Int])
      doBothParts contents = do
          a <- day4part1 contents
          b <- day4part2 contents
          return (a, b)