module DayTwelve where

import qualified Data.Map.Strict as Map
import Numeric (readInt)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)

type PlantState = Map.Map Int Bool
type Rules = Map.Map Int Bool

binaryParser :: String -> Int
binaryParser = fst . head . readInt 2 (`elem` "01") (\a -> if a == '0' then 0 else 1)

choice :: a -> a -> Bool -> a
choice a b True = a
choice a b False = b

hasPlant = (== '#')

boolToBit = choice '1' '0'
boolToPlant = choice '#' '.'

fromPlantStateStr = binaryParser . map (boolToBit . hasPlant)

makeRules :: [String] -> Rules
makeRules = Map.fromList . map ((\[a,b] -> (fromPlantStateStr a, hasPlant $ head b)) . splitOn " => ") . filter (not . null)

makePlantState :: String -> PlantState
makePlantState = Map.fromList . zip [0..] . map hasPlant

parseInitialState :: String -> PlantState
parseInitialState = makePlantState . filter (`elem` ".#")

getSurround :: PlantState -> Int -> Int
getSurround ps i = 
    let
        pts = map (boolToBit . fromMaybe False . flip Map.lookup ps) [i-2..i+2]
    in 
        binaryParser pts

nextStateAt :: Rules -> PlantState -> Int -> Bool
nextStateAt rules ps = fromMaybe False . flip Map.lookup rules . getSurround ps

extend :: PlantState -> PlantState
extend ps =
    let 
        minKey = minimum $ Map.keys ps
        maxKey = maximum $ Map.keys ps
    in
        Map.union ps $ Map.fromList $ zip [minKey - 2, minKey - 1, maxKey + 1, maxKey + 2] $ repeat False

nextState :: Rules -> PlantState -> PlantState
nextState rules plantState = 
    let 
        plantState' = extend plantState 
    in
        Map.mapWithKey (\i _ -> nextStateAt rules plantState' i) plantState'

showState :: PlantState -> String
showState = map boolToPlant . Map.elems

sumPlants :: PlantState -> Int
sumPlants = sum . Map.mapWithKey (curry fst) . Map.filter id

transitionBy :: Int -> Rules -> PlantState -> PlantState
transitionBy 0 _ s = s
transitionBy n rules initialState = transitionBy (n-1) rules (nextState rules initialState)

part1 rules = sumPlants . transitionBy 20 rules
part2 rules initialState =
    let 
        (diff, iter, total) = findSteadyState rules initialState 5 0 [] 0
    in 
        (5000000000 - iter) * diff + total

findSteadyState rules state nRepeats prev diffs i =
    let
        state' = nextState rules state
        curr = sumPlants state'
        diff = (curr - prev)
    in 
        case diffs of 
            [] -> findSteadyState rules state' nRepeats curr [diff] (i+1)
            (h:rst) | h == diff -> if length rst + 2 >= nRepeats then (diff, i+1, curr) else findSteadyState rules state' nRepeats curr (diff:h:rst) (i+1)
                    | otherwise -> findSteadyState rules state' nRepeats curr [diff] (i+1)

showEach rules state until step prev = if until == 0
    then return ()
    else do
        let state' = transitionBy step rules state
            curr = sumPlants state'
        print curr
        print (curr - prev)
        showEach rules state' (until - 1) step curr