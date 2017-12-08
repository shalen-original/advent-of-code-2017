module Day5 where

import Data.List
import Control.DeepSeq
import Data.Sequence as Seq

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

formatInput :: String -> Seq Int
formatInput = Seq.fromList . (map read) . lines 

-- Answer to part one
part1 :: Seq Int -> Int
part1 lst = numberOfJumps lst 0 0

numberOfJumps :: Seq Int -> Int -> Int -> Int
numberOfJumps seq currPos acc 
    | newPos < 0 || newPos >= (Seq.length seq) = acc + 1
    | otherwise = numberOfJumps newSeq newPos (acc + 1)
    where newPos = currPos + (Seq.index seq currPos)
          newSeq = Seq.adjust (\el -> el + 1) currPos seq

-- Answer to part two
part2 :: Seq Int -> Int
part2 lst = numberOfJumps2 lst 0 0

numberOfJumps2 :: Seq Int -> Int -> Int -> Int
numberOfJumps2 seq currPos acc 
    | newPos < 0 || newPos >= (Seq.length seq) = acc + 1
    | otherwise = numberOfJumps2 newSeq newPos (acc + 1)
    where newPos = currPos + (Seq.index seq currPos)
          newSeq = Seq.adjust (\el -> if el >= 3 then el - 1 else el + 1) currPos seq
