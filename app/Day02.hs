module Day02 where

import Data.Char
import Data.List.Split
import Data.Function
import Debug.Trace

main :: IO ()
main = do
    content <- readFile "resources/day02.txt"
    let contentList = map read (splitOn "," content)
        (x : y : z : xs) = contentList
        contentListReplace = (x : 12 : 2 : xs)

    print $ "star 1: " ++ show (head (processCode 0 $ contentListReplace)) 
    print $ "star 2: " 
        ++ let (noun : verb) = concat (filter (\x ->
                let newList = (head contentList) : x ++ (drop 3 contentList)
                in (head (processCode 0 $ newList)) == 19690720
                ) [[x, y] | x <- [1..99], y <- [1..99]])
            in show (noun * 100 + (verb !! 0))


processCode :: Int -> [Int] -> [Int]
processCode offset input
    | (head (drop offset input)) == 99 = input
    | otherwise = 
        let offsetInput = drop offset input
            (a : b : c : d : xs) = offsetInput
        in
            if a == 1
                then 
                    let calculated = (input !! b) + (input !! c)
                        newInput = replaceNth d calculated input
                    in 
                        processCode (offset + 4) newInput
            else if a == 2
                then 
                    let calculated = (input !! b) * (input !! c)
                        newInput = replaceNth d calculated input
                    in 
                        processCode (offset + 4) newInput
            else 
                []

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

debug = flip trace
