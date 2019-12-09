module Day07 where

import Data.Char
import Data.List.Split
import Data.List
import qualified Data.Map as Map
import Data.Function
import Debug.Trace

main :: IO ()
main = do
    content <- readFile "resources/day07.txt"
    let program = map read (splitOn "," content)
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
            modeParam1 = getMode instructionHead 1
            modeParam2 = getMode instructionHead 2
                -- `debug` ("\nopcode : " ++ show ( opcode) )
        in
            case opcode of
                1 -> sum1 program offset phases amp input output instructionPointers programStates
                    -- let instructionList = drop offset program
                    --     (a : b : c : d : xs) = instructionList
                    --     calculated = 
                    --         executeArithmeticInstruction instructionList program (+) modeParam1 modeParam2

                    --     newProgram = replaceNth d calculated program
                    -- in 
                    --     processCode phases amp input output (offset + 4) newProgram instructionPointers programStates
                2 -> multiply program offset phases amp input output instructionPointers programStates
                3 ->
                    let (a : b : xs) = drop offset program
                        inputValue = head input
                        newProgram = replaceNth b inputValue program
                    in 
                        processCode phases amp (drop 1 input) output (offset + 2) newProgram instructionPointers programStates
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
            -- Jump if True
                5 -> executeBinaryOperator phases amp program instructionPointers (/=) offset input output modeParam1 modeParam2 programStates
            -- Jump if False
                6 -> executeBinaryOperator phases amp program instructionPointers (==) offset input output modeParam1 modeParam2 programStates
            -- Less than
                7 -> executeCompareOperator phases amp program instructionPointers (<) offset input output modeParam1 modeParam2 programStates
            -- Equal
                8 -> executeCompareOperator phases amp program instructionPointers (==) offset input output modeParam1 modeParam2 programStates


executeCompareOperator ::
    [Int] -> Int -> [Int] -> [Int] -> (Int -> Int -> Bool) -> Int -> [Int] -> Int -> Int -> Int -> [[Int]] -> Int 
executeCompareOperator phases amp program instructionPointers operation offset input output modeParam1 modeParam2 programStates =
    let programAtOffset = drop offset program
        (a : b : c : d : xs) = programAtOffset
    in
        if modeParam1 == 1 then
            if modeParam2 == 1 then
                if operation b c then
                    let newProgram = replaceNth d 1 program
                    in processCode phases amp input output (offset + 4) newProgram instructionPointers programStates
                else 
                    let newProgram = replaceNth d 0 program
                    in processCode phases amp input output (offset + 4) newProgram instructionPointers programStates
            else if modeParam2 == 0 then
                if operation b (program !! c) then
                    let newProgram = replaceNth d 1 program
                    in processCode phases amp input output (offset + 4) newProgram instructionPointers programStates
                else 
                    let newProgram = replaceNth d 0 program
                    in processCode phases amp input output (offset + 4) newProgram instructionPointers programStates
            else 
                error $ "--modeParam2 does not exist: " ++ (show modeParam2)

        else if modeParam1 == 0 then
            if modeParam2 == 1 then
                if operation (program !! b) c then
                    let newProgram = replaceNth d 1 program
                    in processCode phases amp input output (offset + 4) newProgram instructionPointers programStates
                else 
                    let newProgram = replaceNth d 0 program
                    in processCode phases amp input output (offset + 4) newProgram instructionPointers programStates
            else if modeParam2 == 0 then
                if operation (program !! b) (program !! c) then
                    let newProgram = replaceNth d 1 program
                    in processCode phases amp input output (offset + 4) newProgram instructionPointers programStates
                else 
                    let newProgram = replaceNth d 0 program
                    in processCode phases amp input output (offset + 4) newProgram instructionPointers programStates
            else 
                error $ "--modeParam2 does not exist: " ++ (show modeParam2)
        else 
            error $ "--modeParam1 does not exist: " ++ (show modeParam1)



executeBinaryOperator :: 
    [Int] -> Int -> [Int] -> [Int] -> (Int -> Int -> Bool) -> Int -> [Int] -> Int -> Int -> Int -> [[Int]] -> Int 
executeBinaryOperator phases amp program instructionPointers operation offset input output modeParam1 modeParam2 programStates =
    let programAtOffset = drop offset program
        (a : b : c : xs) = programAtOffset
    in
        if modeParam1 == 0 then
            if operation (program !! b) 0 then 
                if modeParam2 == 0
                    then processCode phases amp input output (program !! c) program instructionPointers programStates
                else if modeParam2 == 1
                    then processCode phases amp input output c program instructionPointers programStates
                else error $ "--Opcode does not exist: "
            else processCode phases amp input output (offset + 3) program instructionPointers programStates

        else if modeParam1 == 1 then
            if operation b 0 then
                if modeParam2 == 0
                    then processCode phases amp input output (program !! c) program instructionPointers programStates
                else if modeParam2 == 1
                    then processCode phases amp input output c program instructionPointers programStates
                else error $ "--Opcode does not exist: "
            else processCode phases amp input output (offset + 3) program instructionPointers programStates
        else error $ "--modeParam2 does not exist: " ++ (show modeParam2)



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

        -- instructionHead = head (drop ip program)
        -- modeParam1 = getMode instructionHead 1
        -- modeParam2 = getMode instructionHead 2

        -- instructionList = drop ip program
        -- (a : b : c : d : xs) = instructionList
        -- calculated = 
        --     executeArithmeticInstruction instructionList program (+) modeParam1 modeParam2

        newProgram = replaceNth param3 calculated program
    in
        processCode phases amp input output (ip + 4) newProgram instructionPointers programStates



-- executeArithmeticInstruction :: [Int] -> [Int] -> (Int -> Int -> Int) -> Int -> Int -> Int
-- executeArithmeticInstruction instructionList program operation modeParam1 modeParam2 =
--     let (a : b : c : d : xs) = instructionList
--     in 
--         if modeParam1 == 1 then
--             if modeParam2 == 1 then
--                 operation b c
--             else if modeParam2 == 0 then
--                 operation b (program !! c)
--             else 
--                 error $ "--modeParam2 does not exist: " ++ (show modeParam2)
--         else if modeParam1 == 0 then
--             if modeParam2 == 1 then
--                 operation (program !! b) c
--             else if modeParam2 == 0 then
--                 operation (program !! b) (program !! c)
--             else 
--                 error $ "--modeParam2 does not exist: " ++ (show modeParam2)
--         else 
--             error $ "--modeParam1 does not exist: " ++ (show modeParam1)


replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs


digits :: Int -> [Int]
digits 0 = []
digits number = digits (number `div` 10) ++ [number `mod` 10]

debug = flip trace