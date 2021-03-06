module Day13 where

import Data.Char
import Data.List.Split
import Data.List
import qualified Data.Map as Map
import Data.Function
import Debug.Trace

type Base = Int
type Drawing = [Int]
--type Program = Int -> [Int]
type InstructionPointer = Int
type Memory = [Int]
type Program = InstructionPointer -> Memory

--type Drawing = Map.Map (Int, Int) Int
--type Geit = Base -> Drawing

main :: IO ()
main = do
  content <- readFile "resources/day09.txt"
  let program = map read (splitOn "," content)
  let input = program ++ replicate 30000 0
  
  let programOutput = processCode 0 [] 0 input 0
  let chunks = chunksOf 3 programOutput
  let star1 = filter (\x -> last x == 2) chunks
  
--  print $ "star 1: " ++ show (length star1)
  
  print $ "test day 09: " ++ show (2745604242 == (head (processCode 1 [] 0 input 0)))
--  print $ processCode 2 [] 0 input 0
--  print $ "(will take a while) star 2: " ++ show (head (processCode 2 [] 0 input 0))


processCode :: Int -> [Int] -> Int -> [Int] -> Base -> Drawing
--processCode :: Int -> [Int] -> Int -> [Int] -> Base -> Drawing
processCode input output ip program base =
  case head (drop ip program) `mod` 100 of
    1 -> sum1 program ip input output base
    2 -> multiply program ip input output base
    3 -> input' program ip input output base
    4 -> output' program ip input output base
    5 -> jumpIfTrue program ip input output base
    6 -> jumpIfFalse program ip input output base
    7 -> lessThan program ip input output base
    8 -> equals program ip input output base
    9 -> adjustBase program ip input output base
    99 -> output


adjustBase :: [Int] -> Int -> Int -> [Int] -> Base -> Drawing
adjustBase program ip input output base = 
  processCode input output (ip + 2) program newBase
  where
    param1 = getParam program ip 1 base 
    newBase = base + param1


input' :: [Int] -> Int -> Int -> [Int] -> Base -> Drawing
input' program ip input output base =
    let
        newProgram = setAt program ip 1 input base
    in
        processCode 0 output (ip + 2) newProgram base


output' :: [Int] -> Int -> Int -> [Int] -> Base -> Drawing
output' program ip input output base =
    processCode 0 (output ++ [param1]) (ip + 2) program base
    where
      param1 = getParam program ip 1 base


lessThan :: [Int] -> Int -> Int -> [Int] -> Base -> Drawing
lessThan program ip input output base =
    let param1 = getParam program ip 1 base
        param2 = getParam program ip 2 base
        newValue = if param1 < param2 then 1 else 0
        newProgram = setAt program ip 3 newValue base
    in
        processCode input output (ip + 4) newProgram base


equals :: [Int] -> Int -> Int -> [Int] -> Base -> Drawing
equals program ip input output base =
    let param1 = getParam program ip 1 base
        param2 = getParam program ip 2 base
        newValue = if param1 == param2 then 1 else 0
        newProgram = setAt program ip 3 newValue base
    in
        processCode input output (ip + 4) newProgram base


jumpIfTrue :: [Int] -> Int -> Int -> [Int] -> Base -> Drawing
jumpIfTrue program ip input output base =
    let param1 = getParam program ip 1 base
        param2 = getParam program ip 2 base

    in
        if param1 /= 0
            then processCode input output param2 program base
        else processCode input output (ip + 3) program base


jumpIfFalse :: [Int] -> Int -> Int -> [Int] -> Base -> Drawing
jumpIfFalse program ip input output base =
    let param1 = getParam program ip 1 base
        param2 = getParam program ip 2 base
    in
        if param1 == 0
            then processCode input output param2 program base
        else processCode input output (ip + 3) program base


getMode :: Int -> Int -> Int
getMode instructionHead offset =
    case offset of
        1 -> (instructionHead `div` 100) `mod` 10
        2 -> (instructionHead `div` 1000) `mod` 10
        3 -> (instructionHead `div` 10000) `mod` 10


getParam :: [Int] -> Int -> Int -> Int -> Int
getParam program ip offset base =
  let paramOffset = ip + offset
      instructionHead = head (drop ip program)
   in case getMode instructionHead offset of
        0 -> program !! (program !! paramOffset)
        1 -> program !! paramOffset
        2 -> program !! ((program !! paramOffset) + base)


sum1 :: [Int] -> Int -> Int -> [Int] -> Base -> Drawing
sum1 program ip input output base =
    let param1 = getParam program ip 1 base
        param2 = getParam program ip 2 base
        calculated = param1 + param2
        newProgram = setAt program ip 3 calculated base
    in
        processCode input output (ip + 4) newProgram base


multiply :: [Int] -> Int -> Int -> [Int] -> Base -> Drawing
multiply program ip input output base =
    let param1 = getParam program ip 1 base
        param2 = getParam program ip 2 base
        calculated = param1 * param2
        newProgram = setAt program ip 3 calculated base
    in
        processCode input output (ip + 4) newProgram base


replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

setAt :: [Int] -> Int -> Int -> Int -> Int -> [Int]
setAt program ip offset value base =
    let instructionHead = head (drop ip program)
        index = (drop ip program) !! offset
    in
        case getMode instructionHead offset of
            0 -> replaceNth index value program
            2 -> replaceNth (index + base) value program


debug = flip trace