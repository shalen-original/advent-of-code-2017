import System.Environment

import System.IO
import qualified Day1 as D1
import qualified Day2 as D2
import qualified Day3 as D3
import qualified Day4 as D4
import qualified Day5 as D5
 
{-
    List of the various modules. Keys are assumed to be
    unique.
-}
modules = [
        ("0", "Exit", putStrLn "\nBye!"),
        ("1", "Day 1 - Inverse Captcha", D1.run),
        ("2", "Day 2 - Corruption Checksum", D2.run),
        ("3", "Day 3 - Spiral Memory", D3.run),
        ("4", "Day 4 - High-Entropy Passphrases", D4.run),
        ("5", "Day 5 - A Maze of Twisty Trampolines, All Alike", D5.run)
    ]

{-
    Utility functions to build a string representing
    all the problems
-}
printProblem (key, desc, _) = "    " ++ key ++ ") " ++ desc ++ "\n" 
printProblems = foldl (\acc x -> acc ++ printProblem x) "" modules 

{-
    Runs the chosen problem
-}
pickChoosenProblem key = head $ filter (\(k, _, _) -> k == key) modules
runProblem (_, _, fn) = fn
runProblems key = runProblem $ pickChoosenProblem key

main :: IO ()
main = do
    putStrLn "Advent of Code 2017"
    putStrLn "By Matteo Nardini"
    putStrLn "-------------------------\n"

    putStrLn "The following problems are available:" 
    putStrLn $ printProblems
    putStr "What do you want to run: "
    hFlush stdout
    choice <- getChar
    getLine -- To clear the buffer

    putStrLn ""
    runProblems [choice]

