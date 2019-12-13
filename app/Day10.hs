module Day10 where

import Data.List
import qualified Data.Set as Set
import Debug.Trace
import Data.Ratio

type Coord = (Int, Int)

main :: IO ()
main = do
  input <- readFile "resources/day10.txt"
  let planets = coordsList (lines input)
    -- star1
  let relativePlanetsNormalized = map (\planet -> map (\x -> (fst x - fst planet, snd planet - snd x)) planets) planets
  let relativePlanetsNormalizedWithoutSelf = map (filter (/= (0, 0))) relativePlanetsNormalized
  let allRatios = map (map simplifyFraction) relativePlanetsNormalized
  let allRatiosWithoutSelf = map (filter (/= (0, 0))) allRatios
  let allRatiosWithoutSelfSet = map Set.fromList allRatiosWithoutSelf
  let star1 = maximum (map length allRatiosWithoutSelfSet)
    -- star2
  let (Just indexMaxPlanet) = elemIndex star1 (map length allRatiosWithoutSelfSet)
  let maxPlanetCoord = planets !! indexMaxPlanet
  let maxPlanet = relativePlanetsNormalizedWithoutSelf !! indexMaxPlanet
  let maxPlanetGrouped = groupByWholeList (\x y -> simplifyFraction x == simplifyFraction y) maxPlanet
  let maxPlanetSortGroupsInternally =
        map (sortBy (\(x1, x2) (y1, y2) -> compare (abs x1 + abs x2) (abs y1 + abs y2))) maxPlanetGrouped
  let maxPlanetGroupedSortedByRatio =
        sortBy
          (\x y ->
             let ratioA = head x
                 ratioB = head y
                 degreeA = toDegrees ratioA
                 degreeB = toDegrees ratioB
              in compare degreeA degreeB)
          maxPlanetSortGroupsInternally
  let orderByFrontItems = foldl (\x y -> x ++ [head y]) [] maxPlanetGroupedSortedByRatio
  let star2Coords = orderByFrontItems !! 199
  let star2 = ((fst star2Coords + fst maxPlanetCoord) * 100) + (snd maxPlanetCoord - snd star2Coords)
  print $ "star1: " ++ show star1
  print $ "star2: " ++ show star2


toDegrees :: Coord -> Float
toDegrees coord
  | (x < 0) && (y < 0) = degrees + 180
  | y < 0 = degrees + 180
  | x < 0 = degrees + 360
  | otherwise = degrees
  where
    x = fromIntegral (fst coord)
    y = fromIntegral (snd coord)
    degrees = atan (x / y) * (180 / pi)


sortRadial :: Coord -> Coord -> Ordering
sortRadial x y
    | atan (a1/b1) > atan (a2/b2) = GT
    | atan (a1/b1) < atan (a2/b2) = LT
    | atan (a1/b1) == atan (a2/b2) = EQ
    where a1 = fromIntegral (fst x)
          a2 = fromIntegral (snd x)
          b1 = fromIntegral (fst y)
          b2 = fromIntegral (snd y)


groupByWholeList :: (a -> a -> Bool) -> [a] -> [[a]]
groupByWholeList = go [] where
  go acc comp [] = acc
  go acc comp (h:t) =
    let (hs, nohs) = partition (comp h) t
    in go ((h:hs):acc) comp nohs


enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..] 


coordsList :: [String] -> [Coord]
coordsList tiles =
    [(x, y)                              -- generate a Coord pair
        | (y, row) <- enumerate tiles    -- for each row with its coordinate
        , (x, tile) <- enumerate row     -- for each tile in the row (with coordinate)
        , tile == '#']

simplifyFraction :: Coord -> Coord
simplifyFraction coord =
    if snd coord == 0
        then 
            if fst coord == 0
                then (0, 0)
            else 
                let sig = signum (fst coord)
                in 
                    (sig * ((abs (fst coord)) `div` (abs (fst coord))), 0)
    else 
        let sig1 = signum (fst coord)
            sig2 = signum (snd coord)
            simplified = (fst coord) % (snd coord)
        in
            (sig1 * (abs (numerator simplified)), sig2 * (abs (denominator simplified)))

debug = flip trace