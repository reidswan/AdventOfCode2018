module DayThree where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)

data Claim = Claim {
    _id :: Integer, 
    left :: Integer,
    top :: Integer,
    width :: Integer,
    height :: Integer
} deriving (Eq, Show)

splitOn c s = splitOn' c s [] where
    splitOn' c [] a = (a, [])
    splitOn' c (r:rs) a 
        | r == c = (reverse a, rs)
        | otherwise = splitOn' c rs (r:a)

readFirst (a, b) = (read a, b)

parseLine line = 
    let 
        line' = dropWhile (=='#') line
        (_id, postId) = readFirst $ splitOn '@' line'
        (left, postLeft) = readFirst $ splitOn ',' postId
        (top, postTop) = readFirst $ splitOn ':' postLeft
        (width, height) = (\(a, b) -> (read a, read b)) $ splitOn 'x' postTop
    in 
        Claim { _id = _id, left = left, top = top, width = width, height = height }

coveredBy (Claim {_id=i, left=l, top=t, width=w, height=h}) = [((x,y), i) | x <- [l..l+w-1], y <- [t..t+h-1]]

countOverlaps m = length $ Map.filter ((>= 2) . length) m

mapCollect :: (Ord a) => Map.Map a [c] -> [(a, c)] -> Map.Map a [c]
mapCollect = foldl (\m (i,j) -> Map.insert i (j:fromMaybe [] (Map.lookup i m)) m)

partOne = countOverlaps . foldl mapCollect Map.empty . map coveredBy

partTwo claims = 
    let 
        claimsById = Map.fromList [(_id claim, claim) | claim <- claims]
        filtered = Map.filter ((== 1) . length) $ foldl mapCollect Map.empty $ map coveredBy claims
        idsToPoints = mapCollect Map.empty $ map (\(a, b) -> (head b, a)) $ Map.toList filtered
    in
        fst $ head $ Map.toList $ Map.filterWithKey (\k a ->
            Set.fromList a == Set.fromList (fromMaybe [] $ fmap (map fst . coveredBy) $ Map.lookup k claimsById)
        ) idsToPoints
