module AoCLib.SeqRing where

import qualified Data.Sequence as Seq

data SeqRing a = SeqRing {before :: Seq.Seq a, after :: Seq.Seq a} deriving (Eq, Show)

empty :: SeqRing a
empty = SeqRing {before = Seq.empty, after = Seq.empty}

fromList :: [a] -> SeqRing a
fromList l = empty {after = Seq.fromList l}

head :: SeqRing a -> a
head sr = case sr of 
    SeqRing {after = trg Seq.:<| _} -> trg
    SeqRing {before = _ Seq.:|> trg} -> trg
    _ -> error "Called head on an empty SeqRing"

first :: SeqRing a -> a
first SeqRing {before = _ Seq.:|> trg} = trg
first SeqRing {after = trg Seq.:<| _} =  trg
first _ = error "Called first on an empty SeqRing"

last :: SeqRing a -> a
last SeqRing {after = _ Seq.:|> trg} = trg
last SeqRing {before = trg Seq.:<| _} = trg
last _ = error "Called last on an empty SeqRing"

normalize :: SeqRing a -> SeqRing a
normalize SeqRing {after=Seq.Empty, before=b} = SeqRing {after=Seq.reverse b, before=Seq.empty}
normalize sr = sr

insert :: a -> SeqRing a -> SeqRing a
-- head $ insert a b = a
insert elem sr = 
    let 
        sr'@SeqRing {after=a} = normalize sr
    in 
        sr' {after=elem Seq.:<| a}

prepend :: a -> SeqRing a -> SeqRing a
-- first $ prepend a b = a
-- head $ prepend a b = head b
prepend elem sr =
    let
        sr'@SeqRing {before=b} = normalize sr
    in
        sr' {before = b Seq.:|> elem}

append :: a -> SeqRing a -> SeqRing a
-- last $ append a b = a
-- head $ append a b = head b
append elem sr = 
    let
        sr'@SeqRing {after=a} = normalize sr
    in
        sr' {after = a Seq.:|> elem}

rotate :: Integral i => i -> SeqRing a -> SeqRing a
rotate _ (SeqRing Seq.Empty Seq.Empty) = empty
rotate 0 sr = normalize sr
rotate i sr
    | i > 0 = case sr of
        SeqRing {after=Seq.Empty, before=b} -> rotate i SeqRing {after=Seq.reverse b, before=Seq.empty}
        SeqRing {after=trg Seq.:<| a, before=b} -> rotate (i-1) SeqRing {after = a, before = trg Seq.:<| b}
    | i < 0 = case sr of 
        SeqRing {before=Seq.Empty, after=a} -> rotate i SeqRing {before=Seq.reverse a, after=Seq.empty}
        SeqRing {before=trg Seq.:<| b, after=a} -> rotate (i+1) SeqRing {before=b, after= trg Seq.:<| a}
