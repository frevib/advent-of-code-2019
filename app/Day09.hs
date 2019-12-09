module Day07 where

import Data.Char
import Data.List.Split
import Data.List
import qualified Data.Map as Map
import Data.Function
import Debug.Trace

type Program = [Int] -> Int

main :: IO ()
main = do
    content <- readFile "resources/day05.txt"
    let program = map read (splitOn "," content)

    -- let test1 = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99] ++ (replicate 30000 0)
        
    let testEqual = [3,9,8,9,10,9,4,9,99,-1,8]
    let testLessThan = [3,3,1107,-1,8,3,4,3,99]
    let jumpTest = [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]

    let maxValueStar1 = head (processCode 1 [] 0 program)
    let maxValueStar2 = head (processCode 5 [] 0 program)

    let test1 = maxValueStar1 == 9961446
    let test2 = maxValueStar2 == 742621
    

    -- print $ "star 1: " ++ show (head (processCode 1 [] 0 program))
    -- print $ "star 2: " ++ show (head (processCode 5 [] 0 program))
    print $ "tests: " ++ show test1 ++ ", " ++ show test2

-- processCode input output offset program
processCode :: Int -> [Int] -> Int -> [Int] -> [Int]
processCode input output offset program =
    let instructionHead = head (drop offset program) 
        opcode = instructionHead `mod` 10
        modeParam1 = instructionHead `div` 100 `mod` 10
        modeParam2 = instructionHead `div` 1000 `mod` 10
    in
        case (head (drop offset program)) `mod` 100 of
            1 -> sum1 program offset input output
            2 -> multiply program offset input output
            3 -> input' program offset input output
            4 -> output' program offset input output
            5 -> jumpIfTrue program offset input output
            6 -> jumpIfFalse program offset input output 
            7 -> lessThan program offset input output
            8 -> equals program offset input output
            99 -> output

input' :: [Int] -> Int -> Int -> [Int] -> [Int]              
input' program ip input output =
    let (a : b : xs) = drop ip program
        newProgram = replaceNth b input program   
    in 
        processCode 0 output (ip + 2) newProgram


output' :: [Int] -> Int -> Int -> [Int] -> [Int]
output' program ip input output =
    let (a : b : xs) = drop ip program
    in 
        processCode 0 [program !!b] (ip + 2) program


lessThan :: [Int] -> Int -> Int -> [Int] -> [Int] 
lessThan program ip input output =
    let param1 = getParam program ip 1
        param2 = getParam program ip 2
        param3 = (drop ip program) !! 3
        newValue = if param1 < param2 then 1 else 0
        newProgram = replaceNth param3 newValue program
    in
        processCode input output (ip + 4) newProgram


equals :: [Int] -> Int -> Int -> [Int] -> [Int]
equals program ip input output =
    let param1 = getParam program ip 1
        param2 = getParam program ip 2
        param3 = (drop ip program) !! 3
        newValue = if param1 == param2 then 1 else 0
        newProgram = replaceNth param3 newValue program
    in
        processCode input output (ip + 4) newProgram


jumpIfTrue :: [Int] -> Int -> Int -> [Int] -> [Int] 
jumpIfTrue program ip input output =
    let param1 = getParam program ip 1
        param2 = getParam program ip 2
    in 
        if param1 /= 0 
            then processCode input output param2 program
        else processCode input output (ip + 3) program


jumpIfFalse :: [Int] -> Int -> Int -> [Int] -> [Int]
jumpIfFalse program ip input output =
    let param1 = getParam program ip 1
        param2 = getParam program ip 2
    in 
        if param1 == 0 
            then processCode input output param2 program
        else processCode input output (ip + 3) program


getMode :: Int -> Int -> Int
getMode instructionHead offset =
    case offset of
        1 -> (instructionHead `div` 100) `mod` 10
        2 -> (instructionHead `div` 1000) `mod` 10
        3 -> (instructionHead `div` 10000) `mod` 10


getParam :: [Int] -> Int -> Int -> Int
getParam program ip offset =
    let paramOffset = ip + offset
        instructionHead = head (drop ip program)
    in
        case getMode instructionHead offset of
            0 -> (program !! (program !! paramOffset))
            1 -> program !! paramOffset


sum1 :: [Int] -> Int -> Int -> [Int] -> [Int]
sum1 program ip input output =
    let param1 = getParam program ip 1
        param2 = getParam program ip 2
        param3 = (drop ip program) !! 3
        calculated = param1 + param2
        newProgram = replaceNth param3 calculated program
    in
        processCode input output (ip + 4) newProgram


multiply :: [Int] -> Int -> Int -> [Int] -> [Int]
multiply program ip input output =
    let param1 = getParam program ip 1
        param2 = getParam program ip 2
        param3 = (drop ip program) !! 3
        calculated = param1 * param2
            -- `debug` ("\nhello : " ++ show (ip))
        newProgram = replaceNth param3 calculated program
    in
        processCode input output (ip + 4) newProgram


replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs


debug = flip trace