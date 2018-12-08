module DayFour where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Ordering, compare, comparing)
import Data.Foldable (maximumBy)
import Data.List (sort)

data ShiftStatus = Awake | Asleep deriving (Eq, Show)

data Shift = Shift {
    _id :: Integer,
    date :: String,
    minute :: Integer, 
    status :: ShiftStatus
} deriving (Eq, Show)

splitOn c s = splitOn' c s [] where
    splitOn' c [] a = (reverse a, [])
    splitOn' c (r:rs) a 
        | r == c = (reverse a, rs)
        | otherwise = splitOn' c rs (r:a)

isPrefixOf [] b = True
isPrefixOf a [] = False
isPrefixOf (a:as) (b:bs)
 | a == b = isPrefixOf as bs
 | otherwise = False

readFirst (a, b) = (read a :: Integer, b)

parseLines = reverse . fst . foldl (\(acc, prevId) line -> 
    let 
        s = parseLine line prevId 
    in 
        (s:acc, _id s)
    ) ([], -1) . sort

parseLine line prevId = 
    let 
        line' = dropWhile (== '[') line
        (date, dateRest) = splitOn ' ' line'
        (_, _rest) = splitOn ':' dateRest
        (minute, minuteRest) = readFirst $ splitOn ']' _rest
        (status, id') = if " Guard" `isPrefixOf` minuteRest 
                        then (Awake, fst $ readFirst $ splitOn ' ' $ snd $ splitOn '#' minuteRest)
                        else if " falls" `isPrefixOf` minuteRest then (Asleep, prevId)
                        else (Awake, prevId)
    in 
        Shift {_id=id', date=date, minute=minute, status=status}

fillShifts [] = []
fillShifts [s] = [s]
fillShifts (Shift {_id=i, date=d, minute=m, status=s}:Shift {_id=i', date=d', minute=m', status=s'}:shifts) = 
    let
        end = if m'-1 >= m then m' - 1 else 60 + m' - 1
        fills = [Shift {_id=i, date=d, minute=if newM >= 60 then newM - 60 else newM, status=s} | newM <- [m .. end]]
    in 
        fills ++ fillShifts (Shift {_id=i', date=d', minute=m', status=s'}:shifts)

guardSleepMap :: [Shift] -> Map.Map Integer [Integer]
guardSleepMap = Map.fromListWith (++) . map (\s -> (_id s, [minute s])) . filter ((== Asleep) . status) . fillShifts

partOne :: [Shift] -> Integer
partOne = 
    let
        idMinuteMult = uncurry (flip $ (*) . fromMaybe 0 . most)
        maxByMinutesAsleep = maximumBy (\a b -> compare (length $ snd a) (length $ snd b))
    in 
        idMinuteMult . maxByMinutesAsleep . Map.toList . guardSleepMap

partTwo :: [Shift] -> Integer
partTwo = 
    let 
        idMinuteMult = uncurry (flip $ (*) . fst)
        maxAsleepMinute (a, b) = (a, fromMaybe (-1, -1) $ most_ b)
    in 
        idMinuteMult . maximumBy (comparing (snd . snd)) . map maxAsleepMinute . Map.toList . guardSleepMap

most :: (Ord a) => [a] -> Maybe a
most [] = Nothing
most ns = 
    let ns' = sort ns 
        curr = head ns'
    in 
        most' (tail ns') curr 1 []
    where 
        most' [] c n acc = Just $ fst $ maximumBy (comparing snd) ((c, n):acc)
        most' (c:cs) prev n acc
            | c == prev = most' cs prev (n+1) acc
            | otherwise = most' cs c 1 ((prev, n):acc)

most_ :: (Ord a, Integral b) => [a] -> Maybe (a, b)
most_ [] = Nothing
most_ ns = 
    let ns' = sort ns 
        curr = head ns'
    in 
        most' (tail ns') curr 1 []
    where 
        most' [] c n acc = Just $ maximumBy (comparing snd) ((c, n):acc)
        most' (c:cs) prev n acc
            | c == prev = most' cs prev (n+1) acc
            | otherwise = most' cs c 1 ((prev, n):acc)
