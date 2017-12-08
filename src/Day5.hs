module Day5 where

import Data.List
import Control.DeepSeq

run :: IO () 
run = do
    putStrLn "Day 5 - A Maze of Twisty Trampolines, All Alike"
    putStrLn "Reading input from file \"inputs\\Day5.txt\""
    text <- readFile "inputs\\Day5.txt"
    putStr "The answer for part 1 is: "
    putStrLn . show . part1 . formatInput $ text
    putStr "The answer for part 2 is: "
    putStrLn . show . part2 . formatInput $ text
    putStrLn ""

formatInput :: String -> [Int]
formatInput = (map read) . lines 

-- Answer to part one
part1 :: [Int] -> Int
part1 lst = numberOfJumps lst 0 0

numberOfJumps :: [Int] -> Int -> Int -> Int
numberOfJumps lst currPos acc 
    | newPos < 0 || newPos >= (length lst) = acc + 1
    | otherwise = newList `deepseq` (numberOfJumps newList newPos (acc + 1))
    where newPos = currPos + (lst !! currPos)
          newList = (inc lst currPos)

inc :: [Int] -> Int -> [Int]
inc lst currPos = map (\(el, pos) -> if pos == currPos then el + 1 else el)(zip lst [0, 1..])

-- Answer to part two
part2 :: [Int] -> Int
part2 lst = numberOfJumps2 lst 0 0

numberOfJumps2 :: [Int] -> Int -> Int -> Int
numberOfJumps2 lst currPos acc 
    | newPos < 0 || newPos >= (length lst) = acc + 1
    | otherwise = newList `deepseq` (numberOfJumps2 newList newPos (acc + 1))
    where newPos = currPos + (lst !! currPos)
          newList = (inc2 lst currPos)

inc2 :: [Int] -> Int -> [Int]
inc2 lst currPos = map (\(el, pos) -> if pos == currPos then (if el >= 3 then el - 1 else el + 1) else el)(zip lst [0, 1..])
