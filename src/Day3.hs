module Day3 where

import Data.List

type Point = (Int, Int)

run :: IO () 
run = do
    putStrLn "Day 3 - Spiral Memory"
    putStrLn "Reading input from file \"inputs\\Day3.txt\""
    text <- readFile "inputs\\Day3.txt"
    putStr "The answer for part 1 is: "
    putStrLn . show . steps . formatInput $ text
    putStr "The answer for part 2 is: "
    --putStrLn . (show :: Int -> String) . checksumPartTwo . formatInput $ text
    putStrLn ""

formatInput :: String -> Int
formatInput s = read s

-- Answer to part one
steps :: Int -> Int
steps n = l1dist0 . findCoords $ n


-- Answer to part two

--summedElement ((0, 0), 1) = ((0, 0), 1)
--summedElement (p, n) = (p, sum $ map snd (filter (\(pel, _) -> isAround p pel) (zip (take (n-1) summedSpiral) [1, 2..]) ))

isAround :: Point -> Point -> Bool
isAround (x, y) (xel, yel) = ((abs (x - xel)) <= 1) 
                                && ((abs (y - yel)) <= 1) 
                                && (x /= xel || y /= yel)



-- Given the index of a cell in the spiral, returns the cartesian
-- coordinates of that cell
findCoords :: Int -> Point
findCoords n = spiral !! (n - 1)

-- Calculates L1 distance between origin and a point
l1dist0 :: Point -> Int
l1dist0 point = l1dist (0, 0) point

-- Calculates L1 distance between two points
l1dist :: Point -> Point -> Int
l1dist (x1, y1) (x2, y2) = (abs (x1 - x2)) + (abs (y1 - y2))


-- See here: https://stackoverflow.com/questions/398299/looping-in-a-spiral
spiral :: [Point]
spiral = spiralAux (0, 0) 1 1 False False

spiralAux :: Point -> Int -> Int -> Bool -> Bool -> [Point]
spiralAux (x, y) d m rowDone colDone
    | (not rowDone) && 2 * x * d < m = (x, y) : spiralAux (x + d, y) d m False colDone
    | (not rowDone) && 2 * x * d >= m = spiralAux (x, y) d m True colDone
    | (not colDone) && 2 * y * d < m = (x, y) : spiralAux (x, y + d) d m rowDone False
    | (not colDone) && 2 * y * d >= m = spiralAux (x, y) d m rowDone True
    | rowDone && colDone = spiralAux (x, y) (-1 * d) (m + 1) False False
