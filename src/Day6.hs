module Day6 where

import Data.Sequence as Seq
import Data.Maybe

run :: IO () 
run = do
    putStrLn "Day 6 - Memory Reallocation"
    putStrLn "Reading input from file \"inputs\\Day6.txt\""
    text <- readFile "inputs\\Day6.txt"
    putStr "The answer for part 1 is: "
    putStrLn . show . part1 . formatInput $ text
    putStr "The answer for part 2 is: "
    putStrLn . show . part2 . formatInput $ text
    putStrLn ""

formatInput :: String -> Seq Int
formatInput str = Seq.fromList (map (read) (words str))

-- Answer to part one
part1 :: Seq Int -> Int
part1 seq = cycleList seq [] 0

cycleList :: Seq Int -> [Seq Int] -> Int -> Int
cycleList seq lst acc 
    | elem seq lst = acc
    | otherwise = cycleList (doCycle seq) (seq : lst) (acc + 1)

--cycleList seq = seq : (cycleList (doCycle seq))

doCycle :: Seq Int -> Seq Int
doCycle seq = redis (Seq.update indexMax 0 seq) max (mod (indexMax + 1) (Seq.length seq))
                where max = maximum seq
                      indexMax = fromJust (Seq.elemIndexL max seq)

redis :: Seq Int -> Int -> Int -> Seq Int
redis seq 0 _ = seq
redis seq qt currIndex = redis (Seq.adjust (+1) currIndex seq) (qt - 1) (mod (currIndex + 1) (Seq.length seq))


-- Answer to part two
part2 :: Seq Int -> Int
part2 seq = part1 ((iterate (doCycle) seq) !! (part1 seq))

