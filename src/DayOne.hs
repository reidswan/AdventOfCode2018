module DayOne (partOne, partTwo) where

import Data.List (sort)

data Coll = Coll { 
  a :: Integer,
  b :: Integer,
  dist :: Integer
}

partOne numbers = sum numbers
partTwo freqs = let
                    (total, numbers) = foldl (\(acc, b) a -> (a+acc, a+acc:b)) (0, []) freqs
                    -- numbers = reverse numbers'
                    singleSeqRepeat = findSingleSeqRepeat (sort numbers)
                    pairs = [
                      Coll {a = val_j, b = val_i, dist = abs((val_j - val_i) `div` total)} | 
                        val_i <- numbers, 
                        val_j <- numbers,
                        val_i /= val_j,
                        (val_j - val_i) `mod` total == 0]
                  in
                    if total == 0 
                    then Just $ head numbers
                    else case singleSeqRepeat of 
                      Nothing -> (fmap (\c -> if a c == b c + dist c * total then a c else b c) $ foldl (\acc c -> Just (case acc of 
                        Nothing -> c
                        Just x -> if (dist c) < (dist x) then c else x)) Nothing pairs)
                      a -> a

findSingleSeqRepeat (a:b:rest) 
  | a == b = Just a
  | otherwise = findSingleSeqRepeat (b:rest)
findSingleSeqRepeat _ = Nothing
