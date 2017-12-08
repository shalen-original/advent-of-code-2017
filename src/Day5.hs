module Day5 where

import Data.List

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
    | otherwise = numberOfJumps (inc lst currPos) newPos (acc + 1)
    where newPos = currPos + (lst !! currPos)

inc :: [Int] -> Int -> [Int]
inc lst currPos = map (\(el, pos) -> if pos == currPos then el + 1 else el)(zip lst [0, 1..])

-- Answer to part two
part2 :: [Int] -> Int
part2 lst = 2 --length $ filter isValid2 lst 

