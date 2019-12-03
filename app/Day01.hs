module Day01 where

import Data.Char
import Data.List.Split
import Data.Function
import Debug.Trace

main :: IO ()
main = do
    print $ "Star 1: "
    print $ input
        & map read 
        & map calculateFuel
        & foldl (+) 0 
    print $ "Star 2: " 
        ++ show (
            input
            & map read
            & map calculateFuelSeries
            & foldl (+) 0
        )
    

calculateFuel :: Int -> Int
calculateFuel x = ((subtract 2).(`div` 3)) x

calculateFuelSeries :: Int -> Int
calculateFuelSeries x 
    | x < 1 = 0
    | otherwise = 
        let fuel = calculateFuel x
        in (noNegative fuel) + (calculateFuelSeries fuel)

noNegative :: Int -> Int
noNegative x
    | x < 0 = 0
    | otherwise = x

input :: [String]
input = 
    splitOn "," "66016,85415,51706,96238,62503,61186,119728,69237,54386,137211,65936,129665,104711,121892,54525,124849,69572,105755,101883,62077,50281,50495,95260,132134,70793,73917,89493,146104,50175,147230,50620,116767,53185,101222,66030,143778,128607,65723,67468,130416,140049,106694,59226,55728,70444,142973,122909,100359,135329,112347,91435,117106,74651,58224,77738,102308,144916,144013,82723,54674,97031,94405,137411,61090,77394,59677,96188,70689,115428,100885,52367,69072,77555,77178,99734,143520,51012,59534,90749,100616,129295,102922,115119,112385,70400,137409,133502,92323,88413,121153,87291,144171,75094,68871,74335,53503,70425,89354,134887,63969"

debug = flip trace