module Utils where

splitAtNextNewline :: String -> (String, String)
splitAtNextNewline = splitAtNextNewlineTailRec ""
  where
    splitAtNextNewlineTailRec :: String -> String -> (String, String)
    splitAtNextNewlineTailRec acc s =
      case s of
        "" -> (acc, "")
        '\n' : rest -> (acc, rest)
        -- hopefully ++ is tail recursive
        x : xs -> splitAtNextNewlineTailRec (acc ++ [x]) xs

splitAllNewlines :: String -> [String]
splitAllNewlines s =
  let (first, rest) = splitAtNextNewline s
   in case rest of
        "" -> [first] -- nothing else to split
        words -> first : splitAllNewlines rest

stringToIntList :: String -> [Int]
stringToIntList s = map read (splitAllNewlines s)
