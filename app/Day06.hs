module Day06 where

import Data.Char
import Data.List.Split
import Data.List
import qualified Data.Set as Set
import Data.Function
import qualified Data.Map as Map


main :: IO ()
main = do
    input <- readFile "resources/day06.txt"
    let inputLines = lines input
    let edgesMap =
            inputLines
            & map (\x ->
                let item = splitOn ")" x
                in (last item, head item)
            )
            & Map.fromList

    let diff_YOU_SAN =
            length
            $ Set.difference
                (Set.fromList (pathToCOM (Just "YOU") edgesMap))
                (Set.fromList (pathToCOM (Just "SAN") edgesMap))

    let diff_SAN_YOU =
            length
            $ Set.difference
                (Set.fromList (pathToCOM (Just "SAN") edgesMap))
                (Set.fromList (pathToCOM (Just "YOU") edgesMap))

    print $ "star 1: "
        ++ show
            (foldl (\x y -> x + (length (pathToCOM y edgesMap)))
                0
                (map Just (Map.keys edgesMap)))
    print $ "star 1: " ++ show (diff_YOU_SAN + diff_SAN_YOU)


pathToCOM :: Maybe String -> Map.Map String String -> [Maybe String]
pathToCOM (Just key) edgesMap =
    let end = (Map.lookup key edgesMap)
    in
        if end == Just "COM"
            then [Just "COM"]
        else ((pathToCOM end edgesMap) ++ [end] ++ [])

