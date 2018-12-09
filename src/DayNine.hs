module DayNine where 

import qualified Data.Map as Map
import qualified Data.Sequence as Seq

removeAt :: Int -> Seq.Seq a -> (a, Seq.Seq a)
removeAt i ls = 
    let 
        (a, b) = Seq.splitAt i ls
    in 
        case Seq.viewl b of 
            (r Seq.:< rest) -> (r, a Seq.>< rest)
            _ -> (undefined, ls)

step :: Int -> Int -> Int -> Seq.Seq Int -> Map.Map Int Int -> Int -> Int
step limit curr marble circle scores player
    | marble > limit = maximum scores
    | marble `mod` 23 == 0 = 
        let 
            removalIndex = mod (curr-7) (length circle)
            (v, circle') = removeAt removalIndex circle
            scores' = Map.update (\a -> Just $ a + marble + v) player scores
        in
            step limit removalIndex (marble + 1) circle' scores' (mod (player + 1) (length scores'))
    | otherwise = 
        let
            insertionIndex = mod (curr+2) (length circle)
            circle' = Seq.insertAt insertionIndex marble circle
        in 
            step limit insertionIndex (marble + 1) circle' scores (mod (player + 1) (length scores))


partOne players limit = step limit 0 1 (Seq.fromList [0]) (Map.fromList [(i, 0) | i <- [0..players-1]]) 0
