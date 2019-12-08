module Day05 where

import Data.Char
import Data.List.Split
import Data.Function
import Debug.Trace

main :: IO ()
main = do
    content <- readFile "resources/day05.txt"
    let program = map read (splitOn "," content)

    print $ "star 1: " ++ show (head (processCode 1 [] 0 program))
    print $ "star 2: " ++ show (head (processCode 5 [] 0 program))


processCode :: Int -> [Int] -> Int -> [Int] -> [Int]
processCode input output offset program
    | (head (drop offset program)) == 99 = output
    | otherwise = 
        let instructionHead = head (drop offset program) 
            opcode = instructionHead `mod` 10
            modeParam1 = instructionHead `div` 100 `mod` 10
            modeParam2 = instructionHead `div` 1000 `mod` 10
        in
            if opcode == 1
                then 
                    let instructionList = drop offset program
                        (a : b : c : d : xs) = instructionList
                        calculated = 
                            executeArithmeticInstruction instructionList program (+) modeParam1 modeParam2
                        newProgram = replaceNth d calculated program
                    in 
                        processCode 0 output (offset + 4) newProgram
            else if opcode == 2
                then 
                    let instructionList = drop offset program
                        (a : b : c : d : xs) = instructionList
                        calculated = 
                            executeArithmeticInstruction instructionList program (*) modeParam1 modeParam2
                        newProgram = replaceNth d calculated program
                    in 
                        processCode 0 output (offset + 4) newProgram
            else if opcode == 3
                then
                    let (a : b : xs) = drop offset program
                        newProgram = replaceNth b input program
                        
                    in 
                        processCode 0 output (offset + 2) newProgram
            else if opcode == 4
                then
                    let (a : b : xs) = drop offset program
                    in 
                        processCode 0 [program !!b] (offset + 2) program 
            -- Jump if True
            else if opcode == 5 
                then executeBinaryOperator program (/=) offset output modeParam1 modeParam2
            -- Jump if False
            else if opcode == 6 
                then executeBinaryOperator program (==) offset output modeParam1 modeParam2
            -- Less than
            else if opcode == 7
                then executeCompareOperator program (<) offset output modeParam1 modeParam2
            -- Equal
            else if opcode == 8
                then executeCompareOperator program (==) offset output modeParam1 modeParam2
            else
                error $ "--Opcode does not exist: "


executeCompareOperator ::
    [Int] -> (Int -> Int -> Bool) -> Int -> [Int] -> Int -> Int -> [Int]
executeCompareOperator program operation offset output modeParam1 modeParam2 =
    let programAtOffset = drop offset program
        (a : b : c : d : xs) = programAtOffset
    in
        if modeParam1 == 1 then
            if modeParam2 == 1 then
                if operation b c then
                    let newProgram = replaceNth d 1 program
                    in processCode 0 output (offset + 4) newProgram
                else 
                    let newProgram = replaceNth d 0 program
                    in processCode 0 output (offset + 4) newProgram
            else if modeParam2 == 0 then
                if operation b (program !! c) then
                    let newProgram = replaceNth d 1 program
                    in processCode 0 output (offset + 4) newProgram
                else 
                    let newProgram = replaceNth d 0 program
                    in processCode 0 output (offset + 4) newProgram
            else 
                error $ "--modeParam2 does not exist: " ++ (show modeParam2)

        else if modeParam1 == 0 then
            if modeParam2 == 1 then
                if operation (program !! b) c then
                    let newProgram = replaceNth d 1 program
                    in processCode 0 output (offset + 4) newProgram
                else 
                    let newProgram = replaceNth d 0 program
                    in processCode 0 output (offset + 4) newProgram
            else if modeParam2 == 0 then
                if operation (program !! b) (program !! c) then
                    let newProgram = replaceNth d 1 program
                    in processCode 0 output (offset + 4) newProgram
                else 
                    let newProgram = replaceNth d 0 program
                    in processCode 0 output (offset + 4) newProgram
            else 
                error $ "--modeParam2 does not exist: " ++ (show modeParam2)
        else 
            error $ "--modeParam1 does not exist: " ++ (show modeParam1)



executeBinaryOperator :: 
    [Int] -> (Int -> Int -> Bool) -> Int -> [Int] -> Int -> Int -> [Int]
executeBinaryOperator program operation offset output modeParam1 modeParam2 =
    let programAtOffset = drop offset program
        (a : b : c : xs) = programAtOffset
    in
        if modeParam1 == 0 then
            if operation (program !! b) 0 then 
                if modeParam2 == 0
                    then processCode 0 output (program !! c) program
                else if modeParam2 == 1
                    then processCode 0 output c program
                else error $ "--Opcode does not exist: "
            else processCode 0 output (offset + 3) program

        else if modeParam1 == 1 then
            if operation b 0 then
                if modeParam2 == 0
                    then processCode 0 output (program !! c) program
                else if modeParam2 == 1
                    then processCode 0 output c program
                else error $ "--Opcode does not exist: "
            else processCode 0 output (offset + 3) program
        else error $ "--modeParam2 does not exist: " ++ (show modeParam2)


executeArithmeticInstruction :: [Int] -> [Int] -> (Int -> Int -> Int) -> Int -> Int -> Int
executeArithmeticInstruction instructionList program operation modeParam1 modeParam2 =
    let (a : b : c : d : xs) = instructionList
    in 
        if modeParam1 == 1 then
            if modeParam2 == 1 then
                operation b c
            else if modeParam2 == 0 then
                operation b (program !! c)
            else 
                error $ "--modeParam2 does not exist: " ++ (show modeParam2)
        else if modeParam1 == 0 then
            if modeParam2 == 1 then
                operation (program !! b) c
            else if modeParam2 == 0 then
                operation (program !! b) (program !! c)
            else 
                error $ "--modeParam2 does not exist: " ++ (show modeParam2)
        else 
            error $ "--modeParam1 does not exist: " ++ (show modeParam1)


replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs


digits :: Int -> [Int]
digits 0 = []
digits number = digits (number `div` 10) ++ [number `mod` 10]

debug = flip trace