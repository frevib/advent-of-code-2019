module Day11 where

import Data.Char
import Data.List.Split
import Data.List
import qualified Data.Map as Map
import Data.Function
import Debug.Trace

type Base = Int
type Colour = Int
type Grid = Map.Map (Int, Int) Int
type OutputGrid = Map.Map (Int, Int) Int
type Position = (Int, Int)
type Direction = (Int, Int)
data RobotData = Grid Position Direction

main :: IO ()
main = do
  content <- readFile "resources/day11.txt"
  let program = map read (splitOn "," content)
  let programExtended = program ++ replicate 30000 0
  let star2Coords = Map.size (processCode 0 programExtended 1 [] 0 (Map.fromList [((0,0), 0)]) (0,0) (1,0))
  print $ "star 1: " ++ show (Map.size (processCode 0 programExtended 0 [] 0 (Map.fromList [((0,0), 0)]) (0,0) (1,0)))



processCode :: Int -> [Int] -> Int -> [Int] -> Int -> Grid -> Position -> Direction -> OutputGrid
processCode ip program input output base grid position direction =
  case head (drop ip program) `mod` 100 of
    1 -> sum1 program ip input output base grid position direction
    2 -> multiply program ip input output base grid position direction
    3 -> input' program ip input output base grid position direction
    4 -> output' program ip input output base grid position direction
    5 -> jumpIfTrue program ip input output base grid position direction
    6 -> jumpIfFalse program ip input output base grid position direction
    7 -> lessThan program ip input output base grid position direction
    8 -> equals program ip input output base grid position direction
    9 -> adjustBase program ip input output base grid position direction
    99 -> grid



adjustBase :: [Int] -> Int -> Int -> [Int] -> Base -> Grid -> Position -> Direction -> OutputGrid
adjustBase program ip input output base grid position direction =
    let
        param1 = getParam program ip 1 base
        newBase = base + param1
    in
        processCode (ip + 2) program input output newBase grid position direction


input' :: [Int] -> Int -> Int -> [Int] -> Base -> Grid -> Position -> Direction -> OutputGrid
input' program ip input output base grid position direction =
    let
        newProgram = setAt program ip 1 input base
    in
        processCode (ip + 2) newProgram 0 output base grid position direction


output' :: [Int] -> Int -> Int -> [Int] -> Base -> Grid -> Position -> Direction -> OutputGrid
output' program ip input output base grid position direction =
  if null output
    then let newGrid = addToGrid grid position param1
          in processCode (ip + 2) program 0 [param1] base newGrid position direction

    else let newDirection = updateDirection direction param1
             nextPosition = (fst position + fst newDirection, snd position + snd newDirection)
             colorNextPosition = handleJust $ getColour grid nextPosition

          in processCode (ip + 2) program colorNextPosition [] base grid nextPosition newDirection
  where
    param1 = getParam program ip 1 base


updateDirection :: Direction -> Int -> Direction
updateDirection (x, y) turn
  | (y == 1) && (turn == 0) = (-1, 0)
  | (y == 1) && (turn == 1) = (1, 0)
  | (y == -1) && (turn == 0) = (1, 0)
  | (y == -1) && (turn == 1) = (-1, 0)
  | (x == 1) && (turn == 0) = (0, 1)
  | (x == 1) && (turn == 1) = (0, -1)
  | (x == -1) && (turn == 0) = (0, -1)
  | (x == -1) && (turn == 1) = (0, 1)


handleJust :: Maybe Int -> Int
handleJust Nothing = 0
handleJust (Just x) = x


addToGrid :: Grid -> Position -> Colour -> Grid
addToGrid grid position colour = Map.insert position colour grid

getColour :: Grid -> Position -> Maybe Int
getColour grid position = Map.lookup position grid



lessThan :: [Int] -> Int -> Int -> [Int] -> Base -> Grid -> Position -> Direction -> OutputGrid
lessThan program ip input output base grid position direction =
    let param1 = getParam program ip 1 base
        param2 = getParam program ip 2 base
        newValue = if param1 < param2 then 1 else 0
        newProgram = setAt program ip 3 newValue base
    in
        processCode (ip + 4) newProgram input output base grid position direction


equals :: [Int] -> Int -> Int -> [Int] -> Base -> Grid -> Position -> Direction -> OutputGrid
equals program ip input output base grid position direction =
    let param1 = getParam program ip 1 base
        param2 = getParam program ip 2 base
        newValue = if param1 == param2 then 1 else 0
        newProgram = setAt program ip 3 newValue base
    in
        processCode (ip + 4) newProgram input output base grid position direction


jumpIfTrue :: [Int] -> Int -> Int -> [Int] -> Base -> Grid -> Position -> Direction -> OutputGrid
jumpIfTrue program ip input output base grid position direction =
    let param1 = getParam program ip 1 base
        param2 = getParam program ip 2 base

    in
        if param1 /= 0
            then processCode param2 program input output base grid position direction
        else processCode (ip + 3) program input output base grid position direction


jumpIfFalse :: [Int] -> Int -> Int -> [Int] -> Base -> Grid -> Position -> Direction -> OutputGrid
jumpIfFalse program ip input output base grid position direction =
    let param1 = getParam program ip 1 base
        param2 = getParam program ip 2 base
    in
        if param1 == 0
            then processCode param2 program input output base grid position direction
        else processCode (ip + 3) program input output base grid position direction



sum1 :: [Int] -> Int -> Int -> [Int] -> Base -> Grid -> Position -> Direction -> OutputGrid
sum1 program ip input output base grid position direction =
    let param1 = getParam program ip 1 base
        param2 = getParam program ip 2 base
        calculated = param1 + param2
        newProgram = setAt program ip 3 calculated base
    in
        processCode (ip + 4) newProgram input output base grid position direction


multiply :: [Int] -> Int -> Int -> [Int] -> Base -> Grid -> Position -> Direction -> OutputGrid
multiply program ip input output base grid position direction =
    let param1 = getParam program ip 1 base
        param2 = getParam program ip 2 base
        calculated = param1 * param2
        newProgram = setAt program ip 3 calculated base
    in
        processCode (ip + 4) newProgram input output base grid position direction

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


replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs


setAt :: [Int] -> Int -> Int -> Int -> Int -> [Int]
setAt program ip offset value base =
  let instructionHead = head (drop ip program)
      index = drop ip program !! offset
   in case getMode instructionHead offset of
        0 -> replaceNth index value program
        2 -> replaceNth (index + base) value program


debug = flip trace