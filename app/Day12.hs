module Day12 where

import Data.Char
import Data.List.Split
import Data.List
--import qualified Data.Map as Map
import Data.Function
import Debug.Trace

type Moon = [Int]
type Moons = [[Int]]
type Velocity = [Int]
type Velocities = [[Int]]
type Iterations = Int
type Counter = Int
type Energy = Int


main :: IO ()
main = do
    content <- readFile "resources/day12.txt"
    let moons = [[1, 3, -11], [17, -10, -8], [-1, -15, 2], [12, -4, -4]]
    let testMoons = [[-1, 0, 2],
                    [2, -10, -7],
                    [4, -8, 8],
                    [3, 5, -1]]
    let velocities = replicate 4 $ replicate 3 0

    print $ "star1: " ++ show (timeStep moons velocities 1000 1)


timeStep :: Moons -> Velocities -> Iterations -> Counter -> Energy
timeStep moons velocities iterations counter =
  let newVelocities = getNewVelocities moons velocities
      newPositions = applyVelocities moons newVelocities
   in if counter == iterations
        then calculateEnergy newPositions newVelocities
        else timeStep newPositions newVelocities iterations (counter + 1)

calculateEnergy :: Moons -> Velocities -> Energy
calculateEnergy newPositions newVelocities =
  let potA = foldl sumAbs 0 (newPositions !! 0)
      kinA = foldl sumAbs 0 (newVelocities !! 0)
      potB = foldl sumAbs 0 (newPositions !! 1)
      kinB = foldl sumAbs 0 (newVelocities !! 1)
      potC = foldl sumAbs 0 (newPositions !! 2)
      kinC = foldl sumAbs 0 (newVelocities !! 2)
      potD = foldl sumAbs 0 (newPositions !! 3)
      kinD = foldl sumAbs 0 (newVelocities !! 3)
   in potA * kinA + potB * kinB + potC * kinC + potD * kinD

sumAbs :: Int -> Int -> Int
sumAbs x y =
  let absX = abs x
      absY = abs y
  in absX + absY

applyVelocities :: Moons -> Velocities -> Velocities
applyVelocities moons velocities =
  let newPosA = zipWith (+) moonA velocityA
      newPosB = zipWith (+) moonB velocityB
      newPosC = zipWith (+) moonC velocityC
      newPosD = zipWith (+) moonD velocityD
   in
      [newPosA, newPosB, newPosC, newPosD]
  where
      (moonA : moonB : moonC : moonD : _) = moons
      (velocityA : velocityB : velocityC : velocityD : _) = velocities

--getNewVelocities :: Moons -> Velocities -> Velocities
--getNewVelocities moons velocities =
--  let newVelocityA = applyGravity moonA [moonB, moonC, moonD]
--      newVelocityB = applyGravity moonB [moonA, moonC, moonD]
--      newVelocityC = applyGravity moonC [moonA, moonB, moonD]
--      newVelocityD = applyGravity moonD [moonA, moonB, moonC]
--   in
--      [newVelocityA, newVelocityB, newVelocityC, newVelocityD]
--  where
--      (moonA : moonB : moonC : moonD : _) = moons
--      (velocityA : velocityB : velocityC : velocityD : _) = velocities



getNewVelocities :: Moons -> Velocities -> Velocities
--getNewVelocities [] [] = [[]]
getNewVelocities moons velocities =
  let newVelocityA = zipWith (+) (applyGravity moonA [moonB, moonC, moonD]) velocityA
      newVelocityB = zipWith (+) (applyGravity moonB [moonA, moonC, moonD]) velocityB
      newVelocityC = zipWith (+) (applyGravity moonC [moonA, moonB, moonD]) velocityC
      newVelocityD = zipWith (+) (applyGravity moonD [moonA, moonB, moonC]) velocityD
   in
      [newVelocityA, newVelocityB, newVelocityC, newVelocityD]
  where
      (moonA : moonB : moonC : moonD : _) = moons
      (velocityA : velocityB : velocityC : velocityD : _) = velocities



applyGravity :: Moon -> Moons -> Velocity
applyGravity moon (moonA : moonB : moonC : _) =
  let velocityMoonA = zipWith zipGravity moon moonA
      velocityMoonB = zipWith zipGravity moon moonB
      velocityMoonC = zipWith zipGravity moon moonC
  in zipWith (+) velocityMoonA velocityMoonB & zipWith (+) velocityMoonC


zipGravity :: Int -> Int -> Int
zipGravity baseMoon otherMoon
  | baseMoon == otherMoon = 0
  | baseMoon < otherMoon = 1
  | baseMoon > otherMoon = -1