module Day1 where

import Data.Char(digitToInt)

run :: IO () 
run = do
    putStrLn "Day 1 - Inverse Captcha"
    putStrLn "Insert the sequence: "
    getLine
    seq <- getLine
    putStr $ "The sum is: " 
    putStrLn . (show :: Int -> String) . doSum $ seq

doSum seq = sum . map (digitToInt) . filterList $ seq

filterList :: String -> String
filterList lst = map fst $ filter (\el -> fst el == snd el) (zippedList)
                where zippedList = zip lst (firstAsLast lst)

-- Takes the first char of a string and puts it as last character
firstAsLast :: String -> String
firstAsLast lst = tail lst ++ [head lst]
