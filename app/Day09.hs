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
    content <- readFile "resources/day07.txt"
    let program = map read (splitOn "," content)

    let test1 = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99] ++ (replicate 30000 0)
        


    let phasesTestStar2 = [8,7,6,5]
    let phases = phasesTestStar2
    let programStates = [program, program, program, program, program]
    let instructionPointers = [0, 0, 0, 0, 0]

    let maxValueStar1 = maximum (map (\x -> 
                    let (a : b : c : d : e) = x
                    in processCode [b, c, d, head e] 0 [a,0] 0 0 program instructionPointers programStates
                ) (permutations [0..4]))
    let maxValueStar2 = maximum (map (\x ->
                    let (a : b : c : d : e) = x
                    in processCode [b, c, d, head e] 0 [a,0] 0 0 program instructionPointers programStates
                ) (permutations [5..9]) )
    let test1 = maxValueStar1 == 929800
    let test2 = maxValueStar2 == 15432220

    print $ "star 1: " ++ show maxValueStar1
    print $ "star 2: " ++ show maxValueStar2
    print $ "tests: " ++ show test1 ++ ", " ++ show test2

processCode :: [Int] -> Int -> [Int] -> Int -> Int -> [Int] -> [Int] -> [[Int]] -> Int
processCode phases amp input output offset program instructionPointers programStates
    | (head (drop offset program)) == 99 = output 
    | otherwise = 
        let instructionHead = head (drop offset program) 
            opcode = instructionHead `mod` 100
                -- `debug` ("\nopcode : " ++ show ( opcode) )
        in
            case opcode of
                1 -> sum1 program offset phases amp input output instructionPointers programStates
                2 -> multiply program offset phases amp input output instructionPointers programStates
                3 -> input' program offset phases amp input output instructionPointers programStates
                4->
                    let (a : b : xs) = drop offset program
                        phase = take 1 phases
                        producedOutput = (program !! b)
                        nextAmp = ((amp + 1) `mod` 5)

                        newProgramStates = replaceNth amp program programStates
                        newInstructionPointers = replaceNth amp (offset + 2) instructionPointers

                        nextProgram = programStates !! nextAmp
                        instructionPointer = (instructionPointers !! nextAmp)
                    in 
                        processCode 
                            (drop 1 phases) nextAmp (phase ++ [producedOutput]) producedOutput instructionPointer nextProgram newInstructionPointers newProgramStates
                            -- `debug` ("\noutput: " 
                            --     ++ show (phase ++ [producedOutput]) 
                            --     ++ "\noffset: " ++ show offset
                            --     ++ "\nnextAmp: " ++ show nextAmp
                            --     ++ "\nnextProgram: " ++ show nextProgram
                            --     ++ "\ninstruction pointers: " ++ show instructionPointers)
                5 -> jumpIfTrue program offset phases amp input output instructionPointers programStates
                6 -> jumpIfFalse program offset phases amp input output instructionPointers programStates 
                7 -> lessThan program offset phases amp input output instructionPointers programStates
                8 -> equals program offset phases amp input output instructionPointers programStates

input' :: [Int] -> Int -> [Int] -> Int -> [Int] -> Int -> [Int] -> [[Int]] -> Int               
input' program ip phases amp input output instructionPointers programStates =
    let 
        param1 = (drop ip program) !! 1
        newProgram = replaceNth param1 inputValue program
        inputValue = head input
    in 
        processCode phases amp (drop 1 input) output (ip + 2) newProgram instructionPointers programStates


lessThan :: [Int] -> Int -> [Int] -> Int -> [Int] -> Int -> [Int] -> [[Int]] -> Int 
lessThan program ip phases amp input output instructionPointers programStates =
    let param1 = getParam program ip 1
        param2 = getParam program ip 2
        param3 = (drop ip program) !! 3
        newValue = if param1 < param2 then 1 else 0
        newProgram = replaceNth param3 newValue program
    in
        processCode phases amp input output ip newProgram instructionPointers programStates


equals :: [Int] -> Int -> [Int] -> Int -> [Int] -> Int -> [Int] -> [[Int]] -> Int 
equals program ip phases amp input output instructionPointers programStates =
    let param1 = getParam program ip 1
        param2 = getParam program ip 2
        param3 = (drop ip program) !! 3
        newValue = if param1 == param2 then 1 else 0
        newProgram = replaceNth param3 newValue program
    in
        processCode phases amp input output ip newProgram instructionPointers programStates


jumpIfTrue :: [Int] -> Int -> [Int] -> Int -> [Int] -> Int -> [Int] -> [[Int]] -> Int 
jumpIfTrue program ip phases amp input output instructionPointers programStates =
    let param1 = getParam program ip 1
        param2 = getParam program ip 2
    in 
        if param1 /= 0 
            then processCode phases amp input output param2 program instructionPointers programStates
        else processCode phases amp input output ip program instructionPointers programStates


jumpIfFalse :: [Int] -> Int -> [Int] -> Int -> [Int] -> Int -> [Int] -> [[Int]] -> Int 
jumpIfFalse program ip phases amp input output instructionPointers programStates =
    let param1 = getParam program ip 1
        param2 = getParam program ip 2
    in 
        if param1 == 0 
            then processCode phases amp input output param2 program instructionPointers programStates
        else processCode phases amp input output ip program instructionPointers programStates


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


sum1 :: [Int] -> Int -> [Int] -> Int -> [Int] -> Int -> [Int] -> [[Int]] -> Int 
sum1 program ip phases amp input output instructionPointers programStates =
    let param1 = getParam program ip 1
        param2 = getParam program ip 2
        param3 = (drop ip program) !! 3
        calculated = param1 + param2
        newProgram = replaceNth param3 calculated program
    in
        processCode phases amp input output (ip + 4) newProgram instructionPointers programStates


multiply :: [Int] -> Int -> [Int] -> Int -> [Int] -> Int -> [Int] -> [[Int]] -> Int
multiply program ip phases amp input output instructionPointers programStates =
    let param1 = getParam program ip 1
        param2 = getParam program ip 2
        param3 = (drop ip program) !! 3
        calculated = param1 * param2
            -- `debug` ("\nhello : " ++ show (ip))
        newProgram = replaceNth param3 calculated program
    in
        processCode phases amp input output (ip + 4) newProgram instructionPointers programStates


replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

debug = flip trace