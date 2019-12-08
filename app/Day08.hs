module Day08 where

import Data.Char
import Data.List.Split
import Data.List
import qualified Data.Map as Map
import Data.Function
import Debug.Trace

main :: IO ()
main = do
    content <- readFile "resources/day08.txt"
    let wide = 25
    let tall = 6
    let sorted = sort (map sort (chunksOf (wide * tall) content))
    let mostZeros = last sorted

    print $ map (\x -> length (filter (== '0') x)) sorted
    print $  (length (filter (== '2') mostZeros)) * (length (filter (== '1') mostZeros))