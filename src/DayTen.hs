module DayTen where

import Data.Ord (comparing)
import Data.List.Split (splitOn)
import Data.Char (isDigit, toLower)
import qualified Data.Map as Map
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Control.Monad (when)

isDigital a = isDigit a || a == '-'

data Point = Point {position :: (Int, Int), velocity :: (Int, Int)} deriving (Eq, Show)

parseLine :: String -> Point
parseLine line = 
    let 
        [[x, y], [vx, vy]] = map ((map (read . filter isDigital)) . splitOn "," . head . splitOn " velocity") $ filter ((== '<') . head) $ splitOn "=" line
    in 
        Point {position=(x,y), velocity=(vx,vy)}

shift :: [Point] -> [Point]
shift points = map (\p -> p {position=bothSub $ position p}) points
    where
        minX = minimum $ map (fst . position) points
        minY = minimum $ map (snd . position) points
        bothSub (x, y) = (x - minX, y - minY)

scale :: [Point] -> [Point]
scale points = map (\p -> p {position=bothDiv $ position p}) points
    where
        maxX = maximum $ map (fst . position) points
        maxY = maximum $ map (snd . position) points
        bothDiv (a, b) = ((25 * a) `div` maxX, (25 * b) `div` maxY)

normalize = scale . shift

transitionBy n (Point (x, y) v@(dx, dy)) = Point {position=(x+n*dx, y+n*dy), velocity=v}

transition = transitionBy 1

show' pts = makeMapString $ Map.fromList $ map (\a -> (position a, '*')) pts
    where
        ((minX, minY), (maxX, maxY)) = bounds pts
        makeMapString m = concat $ intersperse "\n" [[fromMaybe ' ' $ Map.lookup (i, j) m | i <- [minX..maxX]] | j <- [minY..maxY]]


bounds pts = ((minX, minY), (maxX, maxY))
    where
        ps = map position pts
        minX = minimum $ map fst ps
        maxX = maximum $ map fst ps
        minY = minimum $ map snd ps
        maxY = maximum $ map snd ps

fitsWithin x y pts = maxX - minX <= x && maxY - minY <= y
    where
        ((minX, minY), (maxX, maxY)) = bounds pts


solution i points = 
    if fitsWithin 200 50 points 
        then do 
            putStrLn $ show' points
            putStrLn $ show i ++ ": Done?"
            input <- getLine
            when (null input || (toLower $ head input) /= 'y') $ solution (i+1) $ map transition points
        else
            solution (i+1) $ map transition points

