module Day03 where

import Data.List.Split
import Data.Function
import qualified Data.Set as Set


countUntil :: (Int, Int) -> [(Int, Int)] -> Int
countUntil needle steps 
    | (head steps) == needle = 1
    | otherwise = 1 + countUntil needle (drop 1 steps)


createList :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
createList _ [] = []
createList base steps =
    let aap = generatePath base (head steps)
    in aap ++ (createList (last aap) (drop 1 steps))


generatePath :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
generatePath currentLocation step =
    let (x, y) = step
        (curX, curY) = currentLocation
    in 
        if x > 0
            then [ (x + curX, curY) | x <- [1..x] ]
        else if x < 0
            then [ (-x + curX, curY) | x <- [1..(-1 * x)] ]
        else if y > 0
            then [ (curX, y + curY) | y <- [1..y] ]
        else [ (curX, -y + curY) | y <- [1..(-1 * y)] ]


convertToVector :: String -> (Int, Int)
convertToVector step =
    let direction = take 1 step
    in
        if direction == "U"
            then (0, read(drop 1 step))
        else if direction == "D"
            then (0, -1 * read(drop 1 step))
        else if direction == "R"
            then (read(drop 1 step), 0)
        else (-1 * read(drop 1 step), 0)
        

main :: IO ()
main = do
    content <-  readFile "resources/day03.txt"
    let steps = lines content
        firstWire = (splitOn "," (steps !! 0))
        secondWire = (splitOn "," (steps !! 1))

        firstVectors = map convertToVector firstWire
        secondVectors = map convertToVector secondWire

        firstSet = Set.fromList (createList (0, 0) firstVectors)
        secondSet = Set.fromList (createList (0, 0) secondVectors)

        setIntersection = Set.intersection firstSet secondSet

        firstAmountOfSteps = map (\x -> countUntil x (createList (0, 0) firstVectors)) (Set.toList setIntersection)
        secondAmountOfSteps = map (\x -> countUntil x (createList (0, 0) secondVectors)) (Set.toList setIntersection)

    print $ "star1: " ++ show (minimum $ map (\x -> abs (fst x) + abs(snd x)) (Set.toList setIntersection))
    print $ "star2: " ++ show (minimum (zipWith (+) firstAmountOfSteps secondAmountOfSteps))

