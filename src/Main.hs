import System.Environment

import qualified Day1 as D1
 
{-
    List of the various modules. Keys are assumed to be
    unique.
-}
modules = [
        ("0", "Exit", putStrLn "\nBye!"),
        ("1", "Day 1 - Inverse Captcha", D1.run)
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
    putStrLn "\nWhat do you want to run: "
    choice <- getChar
    getLine -- To clear the buffer

    putStrLn "\n\n"
    runProblems [choice]

