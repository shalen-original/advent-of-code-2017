module Day1 where

import Data.Char(digitToInt)

run :: IO () 
run = do
    putStrLn "Day 1 - Inverse Captcha"
    putStrLn "Reading input from file \"inputs\\Day1.txt\""
    seq <- readFile "inputs\\Day1.txt"
    putStr $ "The sum considering only the next one is: " 
    putStrLn . (show :: Int -> String) . doSum 1 $ seq
    putStr $ "The sum considering only the next (length / 2) one is: " 
    putStrLn . (show :: Int -> String) . doSum (div (length seq) 2) $ seq

--doSumNextLength :: String -> Int
doSum :: Int -> String -> Int
doSum pos_ahead seq = sum . map (digitToInt) . filterList pos_ahead $ seq

-- Filters the list, keeping only the digits that have to be summed
filterList :: Int -> String -> String
filterList pos_ahead lst = map fst $ filter (\el -> fst el == snd el) (zippedList)
                where zippedList = zip lst (rotater pos_ahead lst)

-- Rotates the list right by n positions
rotater :: Int -> [a] -> [a]
rotater _ [] = [] 
rotater n xs = zipWith const (drop n (cycle xs)) xs
