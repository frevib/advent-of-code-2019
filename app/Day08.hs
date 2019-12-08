module Day08 where

import Data.List.Split
import Data.List

main :: IO ()
main = do
    content <- readFile "resources/day08.txt"
    let wide = 25
    let tall = 6
    let layers = (chunksOf (wide * tall) content)
    let sorted = sort (map sort layers)
    let leastZeros = last sorted

    let messageNumbers = foldl (\x y -> zipWith zipLayer x y) (replicate (wide * tall) '2') layers
    let messagePixels = map (\x -> if x == '0' then ' ' else '#') messageNumbers

    print $ "star 1: " ++ show ((length (filter (== '2') leastZeros)) * (length (filter (== '1') leastZeros)))
    putStr $ "star 2: \n" ++ unlines (chunksOf 25 messagePixels)

zipLayer :: Char -> Char -> Char
zipLayer x y = if x == '2' then y else x