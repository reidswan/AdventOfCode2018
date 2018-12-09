module DayEight where

import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)

data Node = Node {metadata :: [Int], children :: [Node]}

parseLine :: String -> [Int]
parseLine = map read . filter (not . null) . splitOn " "

nextNode :: [Int] -> (Node, [Int])
nextNode (c:m:rest) = 
    let 
        (nodes, rest') = takeNodes c rest []
        metadata = take m (rest' ++ repeat 0)
        rest'' = drop m rest'
    in 
        (Node {metadata=metadata, children=nodes}, rest'')
    where 
        takeNodes 0 nums acc = (reverse acc, nums)
        takeNodes i nums acc =
            let 
                (node, nums') = nextNode nums 
            in 
                takeNodes (i-1) nums' (node:acc)
nextNode _ = error "Error while getting next node!"

sumMetadata :: Node -> Int
sumMetadata (Node {metadata=m, children=c}) = sum m + (sum $ map sumMetadata c)

partOne = sumMetadata . fst . nextNode . parseLine

(??) :: [a] -> Int -> Maybe a
(??) a b = case drop b a of 
    [] -> Nothing
    d -> Just $ head d

computeNodeValue :: Node -> Int
computeNodeValue (Node {metadata=m, children=c})
 | null c = sum m
 | otherwise = 
    let 
        values = map computeNodeValue c
    in 
        sum (map (fromMaybe 0 . (values ??) . (flip (-)) 1) m)

partTwo = computeNodeValue . fst . nextNode . parseLine