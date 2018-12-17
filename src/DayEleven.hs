module DayEleven where

import Data.Array.Unboxed
import Data.Foldable (maximumBy)
import Data.Ord (comparing)
import Control.Monad (when)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

data Result = Result {maxVal :: Int, maxSize :: Int, prev :: Int} deriving (Show)
type PowerGrid = UArray (Int, Int) Int

power sn x y = 
    let
        id = x + 10
    in 
        (mod (div ((id*y + sn) * id) 100) 10) - 5

powerGrid :: Int -> PowerGrid
powerGrid sn = array ((1,1), (300,300)) [((x, y), power sn x y) | x <- [1..300], y <- [1..300]]

nextLayer grid i x y = sum [grid ! (x', j) | j <- [y..y']] + sum [grid ! (j, y') | j <- [x..x'] ] - grid ! (x',y')
        where 
            x' = x + i - 1
            y' = y + i - 1

initMap :: PowerGrid -> Map.Map (Int,Int) Result
initMap grid = Map.fromList [(c, Result v 1 v) | (c, v) <- assocs grid]

replaceResult :: PowerGrid -> Int -> (Int, Int) -> Result -> Result
replaceResult grid size c@(x,y) (Result maxVal maxSize prev) = 
    let 
        curr = if x + size - 1 <= 300 && y + size - 1 <= 300 
                then prev + nextLayer grid size x y
                else 0
    in 
        Result curr size curr 

updateResult :: PowerGrid -> Int -> (Int, Int) -> Result -> Result
updateResult grid size c@(x,y) (Result maxVal maxSize prev) = 
    let 
        curr = if x + size - 1 <= 300 && y + size - 1 <= 300 
                then prev + nextLayer grid size x y
                else 0
        (maxVal', maxSize') = maximumBy (comparing fst) [(maxVal, maxSize), (curr, size)]
    in 
        Result maxVal' maxSize' curr 

singleStep f grid map size = Map.mapWithKey (f grid size) map

stepThrough f grid = foldr (flip $ singleStep f grid) (initMap grid) . reverse

best :: Map.Map k Result -> (k, Result)
best = maximumBy (comparing (maxVal . snd)) . Map.assocs

part2 grid = best $ stepThrough updateResult grid [1..300]

part1 grid = best $ stepThrough replaceResult grid [1..3]