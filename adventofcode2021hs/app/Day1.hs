module Day1 where

import Utils
import System.IO


-- For each element in an input list a_i,
-- it gives a tuple of a_i and the next element (a_{i + 1})
pairIterator :: [b] -> [(b, b)]
pairIterator l = zip (init l) (tail l)

-- For each element in an input list a_i
-- it gives a triple of (a_i, a_{i + 1}, a_{i + 2})
tripleIterator :: [c] -> [(c, c, c)]
tripleIterator l = zip3 (init (init l)) (tail (init l)) (tail (tail l))

day1part1Soln :: [Int] -> Int
day1part1Soln nums = foldl (\acc (prev, next) -> if next > prev then acc + 1 else acc) 0 (pairIterator nums)

day1part2Soln :: [Int] -> Int
day1part2Soln nums = 
    let x = map (\(a, b, c) -> a + b + c) (tripleIterator nums)
    in day1part1Soln x

day1main :: IO ()
day1main = do
    handle <- openFile "../inputs2021/day1.txt" ReadMode 
    contents <- hGetContents handle
    let nums = stringToIntList contents
    putStr "Day 1 Part 1: "
    print (day1part1Soln nums)
    putStr "Day 1 Part 2: "
    print (day1part2Soln nums)
    return ()