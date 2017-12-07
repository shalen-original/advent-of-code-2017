module Day2 where

run :: IO () 
run = do
    putStrLn "Day 2 - Corruption Checksum"
    putStrLn "Reading input from file \"inputs\\Day2.txt\""
    text <- readFile "inputs\\Day2.txt"
    putStr "The checksum for part 1 is: "
    putStrLn . (show :: Int -> String) . checksum . formatInput $ text

checksum :: [[Int]] -> Int
checksum lst = foldl (\acc x -> acc + (maximum x - minimum x)) 0 lst

formatInput :: String -> [[Int]]
formatInput inp = map (map (read :: String -> Int)) listOfStrings 
                where listOfStrings = map words (lines inp)
