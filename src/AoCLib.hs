module AoCLib where

import qualified Data.List as List

data Ring a = Ring {before :: [a], after :: [a]} deriving (Eq, Show)

empty :: Ring a
empty = Ring { before=[], after=[] }

head :: Ring a -> a 
head Ring {after=a, before=b} = case a of 
    [] -> last b
    _  -> List.head a

head' :: Ring a -> Maybe a
head' Ring {after=a, before=b} = case (a, b) of 
    ([], [])     -> Nothing
    ([], _)      -> Just $ List.last b
    ((trg:_), _) -> Just trg

pop :: Ring a -> (a, Ring a)
pop Ring {after=a, before=b} = case a of
    []          -> let (trg:rest) = reverse b in 
                    (trg, Ring {after=rest, before=[]})
    (trg:rest)  ->  (trg, Ring {after=rest, before=b})

pop' :: Ring a -> (Maybe a, Ring a)
pop' Ring {after=a, before=b} = case (a, b) of
    ([], [])        -> (Nothing, empty)
    ([], _)         -> let (trg:rest) = reverse b in 
                       (Just trg, Ring{after=rest, before=[]})
    ((trg:rest), _) -> (Just trg, Ring{after=rest, before=b})

insert :: a -> Ring a -> Ring a
insert val ring = ring {after=val:after ring}

fromList :: [a] -> Ring a
fromList l = Ring{after=l, before=[]}

rotate :: (Integral i) => i -> Ring a -> Ring a
rotate 0 r = r
rotate _ Ring {after=[], before=[]} = empty
rotate i Ring {after=a, before=b} 
 | i > 0 = case a of 
    [] -> rotate i Ring {before=[], after=reverse b}
    (trg:rest) -> rotate (i-1) Ring {before=trg:b, after=rest}
 | i < 0 = case b of
    [] -> rotate i Ring {before=reverse a, after=[]}
    (trg:rest) -> rotate (i+1) Ring {after=trg:a, before=rest}

