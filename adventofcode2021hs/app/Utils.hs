module Utils where

import Data.Char

lstrip :: String -> String
lstrip = dropWhile isSpace

strip :: String -> String
strip = takeWhile (not . isSpace) . lstrip

splitAtNextCharacter :: Char -> String -> (String, String)
splitAtNextCharacter c = splitAtNextCharacterTailRec ""
  where
    splitAtNextCharacterTailRec :: String -> String -> (String, String)
    splitAtNextCharacterTailRec acc s =
      case s of
        "" -> (acc, "")
        -- hopefully ++ is tail recursive
        x : xs ->
          if x == c
            then (acc, xs)
            else splitAtNextCharacterTailRec (acc ++ [x]) xs

splitAllCharacters :: Char -> String -> [String]
splitAllCharacters c s =
  let (first, rest) = splitAtNextCharacter c s
   in case rest of
        "" -> [first] -- nothing else to split
        words -> first : splitAllCharacters c rest

stringToIntList :: String -> [Int]
stringToIntList s = map read $ lines s
