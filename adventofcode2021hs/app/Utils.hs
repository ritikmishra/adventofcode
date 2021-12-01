module Utils where


splitAtNextNewline :: String -> (String, String)
splitAtNextNewline s =
    case s of
        "" -> ("", "")
        '\n':rest -> ("", rest)
        x:xs -> let (first, rest) = splitAtNextNewline xs in
                (x:first, rest)

splitAllNewlines :: String -> [String]
splitAllNewlines s =
    let (first, rest) = splitAtNextNewline s in
        case rest of
            "" -> [first]
            words -> first:splitAllNewlines rest

stringToIntList :: String -> [Int]
stringToIntList s = map read (splitAllNewlines s)
