module DayFourteen where

import qualified Data.Sequence as Seq
import Data.List (tails)

-- this awesome solution was stolen from u/mstksg 
-- https://www.reddit.com/r/adventofcode/comments/a61ojp/2018_day_14_solutions/ebr5c0q
scoreboard = 3 : 7 : rest
    where 
        rest = next 0 1 (Seq.fromList [3, 7])
        next c1 c2 recipes = 
            let 
                v1 = Seq.index recipes c1
                v2 = Seq.index recipes c2
                total = v1 + v2
                digits = if total < 10 then [total] else [total `div` 10, total `rem` 10]
                recipes' = recipes Seq.>< Seq.fromList digits
                l' = Seq.length recipes'
                c1' = (c1 + v1 + 1) `rem` l'
                c2' = (c2 + v2 + 1) `rem` l'
            in
                 digits ++ next c1' c2' recipes'

partOne :: Int -> String
partOne i = concatMap show $ take 10 $ drop i scoreboard

partTwo :: Int -> Int
partTwo i = match 0 scoreboard
    where 
        digits = map (read . (:[])) $ show i 
        l = length digits
        match j c = if take l c == digits then j else match (j+1) (drop 1 c)
