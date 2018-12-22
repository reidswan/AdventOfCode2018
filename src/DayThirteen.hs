module DayThirteen where

import Data.List (elemIndex, minimumBy)
import Data.Ord (comparing)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)

data Coord = Coord (Int, Int) deriving (Eq, Show)
data Direction = L | F | R deriving (Eq, Ord, Show)
data Cart = Cart { position :: Coord, facing :: Coord, direction :: Direction, _id :: Coord } deriving (Show)
data Track = Straight { from :: Coord, to :: Coord }
    | Curve {  from1 :: Coord, to1 :: Coord, from2 :: Coord, to2 :: Coord }
    | Intersection { left :: Coord, up :: Coord, right :: Coord, down :: Coord} deriving (Show)
type TrackMap = Map.Map Coord Track
type CartMap = Map.Map Coord Cart
data WorldState = WorldState { tracks :: TrackMap, carts :: CartMap } deriving (Show)

instance Ord Coord where
    compare (Coord (a, b)) (Coord (c, d)) = compare (b, a) (d, c)

instance Eq Cart where
    (==) Cart {_id=a} Cart {_id=b} = a == b

instance Ord Cart where
    compare = comparing _id

nextDirection :: Direction -> Direction
nextDirection d = case d of 
    L -> F
    F -> R
    R -> L

dirNumber :: Direction -> Int
dirNumber L = 1
dirNumber F = 2
dirNumber R = 3

matchIndex :: Coord -> [Coord] -> Int
matchIndex m pts = case elemIndex m pts of
    Just i -> i
    _ -> error (show m ++ " does not match any of " ++ show pts)

crossIntersection :: Coord -> Direction -> [Coord]  -> Coord
crossIntersection from dir pts = pts !! ((matchIndex from pts + dirNumber dir) `rem` length pts)

transition :: TrackMap -> Cart -> Cart
transition tracks (Cart pos fac dir id) = case Map.lookup fac tracks of 
        Nothing -> error ("Fell off the track at " ++ show fac)
        Just track -> Cart { position = fac, facing = posTransition track, direction = orient track, _id=id }
    where
        compare' = compare pos
        orient Intersection {} = nextDirection dir
        orient _ = dir
        posTransition (Straight from to) = case (compare' from, compare' to) of 
            (EQ,  _) -> to
            ( _, EQ) -> from
            _        -> error (show pos ++ " matches neither " ++ show from ++ " nor " ++ show to)
        posTransition (Curve f1 t1 f2 t2) = case ((compare' f1, compare' t1), (compare' f2, compare' t2)) of
            ((EQ, _), _) -> t1
            ((_, EQ), _) -> f1
            (_, (EQ, _)) -> t2
            (_, (_, EQ)) -> f2
            _            -> error (show pos ++ " matches none of " ++ show [f1, t1, f2, t2])
        posTransition (Intersection l u r d) = crossIntersection pos dir [l,u,r,d]

tick1 :: WorldState -> Either WorldState Coord
tick1 (WorldState tracks carts) = innerTick (Map.toAscList carts) Map.empty
    where
        innerTick [] carts' = Left $ WorldState tracks carts'
        innerTick ((p@(Coord (x, y)), c):cs) carts' =
            let
                c'@Cart {position=p'@(Coord (x', y'))} = transition tracks c
            in
                case (Map.lookup p' carts, Map.lookup p' carts') of
                    (_, Just _) -> Right p' -- moving into a position that another cart moved into
                    (Just Cart {position=otherP@(Coord (otherX, otherY))}, _)
                     | otherP > p -> Right p' -- moving into their position before they move
                    _ ->  innerTick cs (Map.insert p' c' carts')

tick2 :: WorldState -> Either WorldState Coord
tick2 (WorldState tracks carts) = innerTick (Map.toAscList carts) Map.empty Set.empty
    where
        innerTick [] carts' remove = 
            let 
                carts'' = Map.filter (not . (`Set.member` remove)) carts'
            in 
                case length carts'' of 
                    0 -> error "No carts remaining!"
                    1 -> Right $ position $ head $ Map.elems carts'
                    _ -> Left $ WorldState tracks carts'
        innerTick ((p@(Coord (x, y)), c):cs) carts' remove
            | Set.member c remove = innerTick cs carts' remove
            | otherwise = 
                let
                    c'@Cart {position=p'@(Coord (x', y'))} = transition tracks c
                in
                    case (Map.lookup p' carts, Map.lookup p' carts') of
                        -- move into a position someone else moved into
                        (_, Just c'') -> innerTick cs (Map.delete p' carts') (Set.union remove $ Set.fromList [c, c''])
                        -- move into a position before someone else moved
                        (Just c''@Cart {position=otherP@(Coord (otherX, otherY))}, _)
                          | otherP > p -> innerTick cs carts' (Set.union remove $ Set.fromList [c, c''])
                        _ ->  innerTick cs (Map.insert p' c' carts') remove


go :: (WorldState -> Either WorldState Coord) -> WorldState -> Coord
go tick ws = case tick ws of 
    Left ws' -> go tick ws'
    Right c -> c

allowed :: Char -> Bool
allowed v = v `elem` "><v^|-+/\\"

parseChar :: (Coord, Char) -> (Coord, Track, Maybe Cart)
parseChar (p@(Coord (i, j)), c) = case c of 
    '^'  -> (p, Straight (Coord (i, j-1)) (Coord (i, j+1)), Just $ Cart p (Coord (i, j-1)) L p)
    '|'  -> (p, Straight (Coord (i, j-1)) (Coord (i, j+1)), Nothing)
    'v'  -> (p, Straight (Coord (i, j-1)) (Coord (i, j+1)), Just $ Cart p (Coord (i, j+1)) L p)
    '<'  -> (p, Straight (Coord (i-1, j)) (Coord (i+1, j)), Just $ Cart p (Coord (i-1, j)) L p)
    '-'  -> (p, Straight (Coord (i-1, j)) (Coord (i+1, j)), Nothing)
    '>'  -> (p, Straight (Coord (i-1, j)) (Coord (i+1, j)), Just $ Cart p (Coord (i+1, j)) L p)
    '/'  -> (p, 
        Curve {
            from1 = Coord (i-1, j), to1 = Coord (i, j-1),-- (-/)
            from2 = Coord (i, j+1), to2 = Coord (i+1, j) -- (/-)
        } , Nothing)
    '\\' -> (p,
        Curve {
            from1 = Coord (i-1, j), to1 = Coord (i, j+1), -- (-\)
            from2 = Coord (i, j-1), to2 = Coord (i+1, j) -- (\-)
        }, Nothing)
    '+'  -> (p,
        Intersection {
            left  = Coord (i-1,   j),
            up    = Coord (  i, j-1),
            right = Coord (i+1,   j),
            down  = Coord (  i, j+1)
        }, Nothing)

parse :: [String] -> WorldState
parse ls =
    let 
        zipped = zip [0..] $ map (zip [0..]) ls
        coordLine (j, cs) = map (\(i, c) -> ((Coord (i, j)), c)) cs
        coorded = concatMap coordLine zipped
        filtered = filter (allowed . snd) coorded
        parsed = map parseChar filtered
        tracks = Map.fromList $ map (\(p, t, _) -> (p, t)) parsed
        carts = Map.fromList [(p, c) | (p, _, Just c) <- parsed]
    in 
        WorldState tracks carts


partOne :: [String] -> Coord
partOne = go tick1 . parse

partTwo :: [String] -> Coord
partTwo = go tick2 . parse
