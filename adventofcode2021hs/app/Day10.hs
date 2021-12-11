{-# LANGUAGE LambdaCase #-}

module Day10 where

import Data.List (sort)
import Debug.Trace
import System.IO
import Utils (maybeMap)

data ChunkType = Parens | Square | Curly | Angle deriving (Show, Eq)

data Token = Token {kind :: ChunkType, isOpen :: Bool} deriving (Show, Eq)

parseTokensInLine :: String -> Maybe [Token]
parseTokensInLine = maybeMap parseToken
  where
    parseToken :: Char -> Maybe Token
    parseToken '(' = Just Token {kind = Parens, isOpen = True}
    parseToken ')' = Just Token {kind = Parens, isOpen = False}
    parseToken '[' = Just Token {kind = Square, isOpen = True}
    parseToken ']' = Just Token {kind = Square, isOpen = False}
    parseToken '{' = Just Token {kind = Curly, isOpen = True}
    parseToken '}' = Just Token {kind = Curly, isOpen = False}
    parseToken '<' = Just Token {kind = Angle, isOpen = True}
    parseToken '>' = Just Token {kind = Angle, isOpen = False}
    parseToken _ = Nothing

data ErrorKind
  = IllegalToken Token
  | IncompleteString [ChunkType]
  | Fine
  deriving (Show, Eq)

findFirstIllegalToken :: [ChunkType] -> [Token] -> ErrorKind
findFirstIllegalToken stack tokens =
  case tokens of
    [] -> case stack of
      [] -> Fine
      s -> IncompleteString s
    Token {kind = kind, isOpen = True} : xs -> findFirstIllegalToken (kind : stack) xs
    t@Token {kind = kind, isOpen = False} : xs -> case stack of
      [] -> IllegalToken t
      s : ss ->
        if s == kind
          then findFirstIllegalToken ss xs
          else IllegalToken t

day10part1 :: [[Token]] -> Int
day10part1 tokens =
  let illegalCharacters = concatMap ((\case IllegalToken x -> [kind x]; _ -> []) . findFirstIllegalToken []) tokens
   in sum (map (\case Parens -> 3; Square -> 57; Curly -> 1197; Angle -> 25137) illegalCharacters)

day10part2 :: [[Token]] -> Int
day10part2 tokens =
  let incompleteLines = concatMap ((\case IncompleteString x -> [x]; _ -> []) . findFirstIllegalToken []) tokens
   in let scores = sort $ map calcStackScore incompleteLines
       in let medianIdx = length scores `div` 2
           in scores !! medianIdx
  where
    calcStackScore :: [ChunkType] -> Int
    calcStackScore x =
      foldl
        ( \acc x ->
            (acc * 5) + case x of
              Parens -> 1
              Square -> 2
              Curly -> 3
              Angle -> 4
        )
        0
        x

day10main :: IO ()
day10main = do
  handle <- openFile "../inputs2021/day10.txt" ReadMode
  contents <- hGetContents handle
  case maybeMap parseTokensInLine (lines contents) of
    Nothing -> putStrLn "parse error"
    Just parsedLines -> do
      putStr "Day 10 Part 1: "
      print $ day10part1 parsedLines
      putStr "Day 10 Part 2: "
      print $ day10part2 parsedLines