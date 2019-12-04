module Day04 where

import Data.List.Split
import Data.List
import Data.Function
import qualified Data.Set as Set
import Debug.Trace

main :: IO ()
main = do
    let input = "165432-707912"
        inputClean = splitOn "-" input
        min = read (inputClean !! 0) :: Int
        max = read (inputClean !! 1) :: Int
        rangeString = map show [min..max]
     
    print $ 
        "star 1: " 
            ++ show (length (filter (\x -> hasAdjacentNumbers x && isIncreasing x) rangeString))
    print $ 
        "star 2: " 
        ++ show 
        (
        rangeString
        & filter (\x -> hasTwoAdjacentNumbers x && isIncreasing x)
        & length
        )

isIncreasing :: String -> Bool
isIncreasing pass =
    let sortedPass = sort pass  
    in sortedPass == pass

hasAdjacentNumbers :: String -> Bool
hasAdjacentNumbers pass =
    length (filter (\x -> (length x) > 1) (group pass)) > 0

hasTwoAdjacentNumbers :: String -> Bool
hasTwoAdjacentNumbers pass =
    length (filter (\x -> (length x) == 2) (group pass)) > 0