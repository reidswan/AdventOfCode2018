module DayFive where

import Data.Char (toLower)
import qualified Data.Set as Set
import Data.Ord (comparing)
import Data.Foldable (maximumBy)

isReactive a b = a /= b && toLower a == toLower b

partOne s = length $ collapse s []

totalCollapseRadius :: Char -> String -> String -> Int -> Int
totalCollapseRadius _ [] _ total = total
totalCollapseRadius c (a:rest) fstack total
    | c == toLower a = 
        let
            filtered = dropWhile (== c) rest
            len = length $ takeWhile (uncurry isReactive) $ zip filtered fstack
        in
            totalCollapseRadius c (drop len filtered) (drop len fstack) (total + len)
    | otherwise = totalCollapseRadius c rest (a:fstack) total

collapse :: String -> String -> String
collapse [] fstack = fstack
collapse (a:rest) [] = collapse rest [a]
collapse (a:rest) (b:fstack)
    | isReactive a b = 
        let 
            len = length $ takeWhile (uncurry isReactive) $ zip rest fstack
        in
            collapse (drop len rest) (drop len fstack)
    | otherwise = collapse rest (a:b:fstack)

partTwo s = 
    let
        collapsedResult = collapse s []
        -- polymers = Set.fromList (map toLower collapsedResult)
        (c, _) = maximumBy (comparing snd) $ map (\a -> (a, totalCollapseRadius a collapsedResult [] 0)) ['a'..'z']
    in
        length $ collapse (filter ((/= c) . toLower) collapsedResult) []

