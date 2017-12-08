module Day2 where

import Data.List

run :: IO () 
run = do
    putStrLn "Day 2 - Corruption Checksum"
    putStrLn "Reading input from file \"inputs\\Day2.txt\""
    text <- readFile "inputs\\Day2.txt"
    putStr "The checksum for part 1 is: "
    putStrLn . (show :: Int -> String) . checksum . formatInput $ text
    putStr "The checksum for part 2 is: "
    putStrLn . (show :: Int -> String) . checksumPartTwo . formatInput $ text

-- Part 1
checksum :: [[Int]] -> Int
checksum lst = foldl (\acc x -> acc + (maximum x - minimum x)) 0 lst

formatInput :: String -> [[Int]]
formatInput inp = map (map (read :: String -> Int)) listOfStrings 
                where listOfStrings = map words (lines inp)

-- Part 2

allPairs :: [Int] -> [(Int, Int)]
allPairs lst =  concat $ map (\i -> map (\j -> (i, j)) (delete i lst)) lst

keepDivisiblePairs :: [(Int, Int)] -> [(Int, Int)]
keepDivisiblePairs lst = filter (\(a, b) -> mod a b == 0) lst

divideAndSum :: [(Int, Int)] -> Int
divideAndSum lst = sum $ map (\(a, b) -> div a b) lst

checksumPartTwo :: [[Int]] -> Int
checksumPartTwo lst = sum $ map (divideAndSum . keepDivisiblePairs . allPairs) lst
