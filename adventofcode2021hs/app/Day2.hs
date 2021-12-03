module Day2 where

import Data.List
import System.IO
import Utils

data SubmarineInstruction
  = Forward Int
  | Down Int
  | Up Int
  deriving (Show)

data Position = Position
  { horiz :: Int,
    vert :: Int,
    aim :: Int
  }
  deriving (Show)

parseInstructions :: [String] -> Maybe [SubmarineInstruction]
parseInstructions [] = Just []
parseInstructions (first : xs) =
  let (kind, num) = splitAtNextCharacter ' ' first
   in do
        currentSubInstruction <-
          ( case kind of
              "forward" -> Just (Forward (read num))
              "down" -> Just (Down (read num))
              "up" -> Just (Up (read num))
              _ -> Nothing
            )
        rest <- parseInstructions xs
        return (currentSubInstruction : rest)

day2Part1 :: [SubmarineInstruction] -> Int
day2Part1 instructions =
  let position =
        foldl
          ( \acc el ->
              ( case el of
                  Forward x -> acc {horiz = x + horiz acc}
                  Down x -> acc {vert = x + vert acc}
                  Up x -> acc {vert = vert acc - x}
              )
          )
          (Position {horiz = 0, vert = 0, aim = 0})
          instructions
   in -- in position
      horiz position * vert position

day2Part2 :: [SubmarineInstruction] -> Int
day2Part2 instructions =
  let position =
        foldl
          ( \acc el ->
              ( case el of
                  Forward x -> acc {horiz = x + horiz acc, vert = vert acc + (aim acc * x)}
                  Down x -> acc {aim = aim acc + x}
                  Up x -> acc {aim = aim acc - x}
              )
          )
          (Position {horiz = 0, vert = 0, aim = 0})
          instructions
   in horiz position * vert position

day2main :: IO ()
day2main = do
  handle <- openFile "../inputs2021/day2.txt" ReadMode
  contents <- hGetContents handle
  let instructions = (parseInstructions . lines) contents
  case instructions of
    Nothing -> do
      putStrLn "error parsing instructions"
      return ()
    Just instr -> do
      putStrLn "Day 1 Part 1 answer: "
      print $ day2Part1 instr
      putStrLn "Day 1 Part 2 answer: "
      print $ day2Part2 instr