module DaySix where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Ord (compare, comparing)
import Data.Foldable (minimumBy, maximumBy)
import Data.Maybe (fromMaybe)

splitOn c [] acc = (reverse acc, [])
splitOn c (a:rest) acc
    | a == c = (reverse acc, rest)
    | otherwise = splitOn c rest (a:acc)

parseLine :: String -> (Int, Int)
parseLine s =
    let 
        (pre, post) = splitOn ',' s []
    in 
        (read pre, read post)

normalizePoints pts = 
    let
        (x_min, y_min) = foldl1 (\(a, b) (c, d) -> (min a c, min b d)) pts
    in
        map (\(a, b) -> (a - x_min, b - y_min)) pts

manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

manhattanClosest (p, distP) (q, distQ) = case compare distP distQ of 
    LT -> (p, distP)
    GT -> (q, distQ)
    EQ -> ((-1, -1), distP)

manhattanSum (p, distP) (q, distQ) = (p, distP + distQ)

forgeGrid f x_max y_max pts = [(x, y, foldl1 f $ map (\a -> (a, manhattanDistance (x, y) a)) pts) | x <- [0..x_max], y <- [0..y_max]]

infiniteMembers x_max y_max grid = Set.fromList $ ((-1, -1):(map (\(_, _, (p, _)) -> p) . filter (\(x, y, _) -> x == x_max || x == 0 || y == y_max || y == 0)) grid)

finiteAreas infinites = filter (\(_, _, (p, _)) -> Set.notMember p infinites)

areaSizes :: [(Int, Int, ((Int, Int), Int))] -> Map.Map (Int, Int) Int
areaSizes = Map.fromListWith (+) . map (\(_, _, (p, _)) -> (p, 1))

partOne pts = 
    let 
        normalizedPoints = normalizePoints pts
        (x_max, y_max) = foldl1 (\(a, b) (c, d) -> (max a c, max b d)) normalizedPoints
        grid = forgeGrid manhattanClosest x_max y_max normalizedPoints
        infinites = infiniteMembers x_max y_max grid
    in 
        maximum $ map snd $ Map.toList $ areaSizes $ finiteAreas infinites grid

partTwo pts = 
    let
        normalizedPoints = normalizePoints pts
        (x_max, y_max) = foldl1 (\(a, b) (c, d) -> (max a c, max b d)) normalizedPoints
        grid = forgeGrid manhattanSum x_max y_max normalizedPoints
    in
        length $ filter (\(_, _, (_, v)) -> v < 10000) grid
