module DayFourteen where

import qualified Data.Sequence as Seq
import qualified AoCLib.SeqRing as SR
import Debug.Trace (trace)

type Chefs = (SR.SeqRing Word, SR.SeqRing Word)

step :: Chefs -> Chefs
step (c1, c2) = 
    let 
        v1 = SR.head c1
        v2 = SR.head c2
        t = v1 + v2
        digits = if t >= 10 then [t `rem` 10, t `div` 10] else [t]
        c1' = SR.rotate (1 + v1) $ foldr (SR.append) c1 digits
        c2' = SR.rotate (1 + v2) $ foldr (SR.append) c2 digits
    in 
        (c1', c2')

go 0 x = x
go i x = trace (show x) $ go (i-1) $ step x

init :: Chefs
init = (SR.fromList [3, 7], SR.fromList [3, 7])