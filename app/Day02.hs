module Day02 where

import Data.Char
import Data.List.Split
import Data.Function
import Debug.Trace

main :: IO ()
main = do
    content <- readFile "resources/day02.txt"
    -- let contentList = ["1","0","0","3"]
    -- let contentList = ["1","1","1","4","99","5","6","0","99"]
    let contentList = splitOn "," content
    let (x : y : z : xs) = contentList
    let contentListReplace = (x : "12" : "2" : xs)
    print $ "star 1: " ++ show (head (processCode 0 $ map read contentListReplace))


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