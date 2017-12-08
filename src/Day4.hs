module Day4 where

import Data.List

run :: IO () 
run = do
    putStrLn "Day 4 - High-Entropy Passphrases"
    putStrLn "Reading input from file \"inputs\\Day4.txt\""
    text <- readFile "inputs\\Day4.txt"
    putStr "The answer for part 1 is: "
    putStrLn . show . part1 . formatInput $ text
    putStr "The answer for part 2 is: "
    putStrLn . show . part2 . formatInput $ text
    putStrLn ""

formatInput :: String -> [[String]]
formatInput = (map words) . lines 

-- Answer to part one
part1 :: [[String]] -> Int
part1 lst = length $ filter isValid1 lst 

isValid1 :: [String] -> Bool
isValid1 lst = (length lst) == (length (nub lst)) 

-- Answer to part two
part2 :: [[String]] -> Int
part2 lst = length $ filter isValid2 lst 

isValid2 :: [String] -> Bool
isValid2 lst = isValid1 $ map sort lst
