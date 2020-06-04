module Decisions where

import Data.List as L
import Game

-- all functions must return Bool

------------ Flush control

-- точное кол-во карт определенной масти
suitNum :: [Card] -> Int -> Int -> Bool 
suitNum cs num suit = num <= foldr (\x k -> if x == suit then k + 1 else k) 0 (fmap fst cs)

isFlush :: [Card] -> Bool
isFlush cs = any (== True) $ fmap (suitNum cs 5) [1..4]

---- Number combs control

-- кол-во карт определенного ранга
isNumRank :: [Card] -> Int -> Int -> Bool     
isNumRank xs k n = length (filter (== n) (fmap snd xs)) == k

---  Care

isCare :: [Card] -> Bool
isCare cs = any (== True) $ fmap (isNumRank cs 4) [2..14] 

---  Set

isSet :: [Card] -> Bool
isSet  cs = any (== True) $ fmap (isNumRank cs 3) [2..14] 

--- Pair

isPair :: [Card] -> Bool
isPair cs = any (== True) $ fmap (isNumRank cs 2) [2..14]

--- Full-House

isFull :: [Card] -> Bool
isFull cs = (isPair cs) && (isSet cs)

------------ Straight control

isStraight :: [Card] -> Bool
isStraight cs =
  let
    xs = countRanks cs
  in 
    (all (>0) $ take 5 xs) ||
    (all (>0) $ drop 1 $ take  6 xs) ||
    (all (>0) $ drop 2 $ take  7 xs) ||
    (all (>0) $ drop 3 $ take  8 xs) ||
    (all (>0) $ drop 4 $ take  9 xs) ||
    (all (>0) $ drop 5 $ take 10 xs) ||
    (all (>0) $ drop 6 $ take 11 xs) ||
    (all (>0) $ drop 7 $ take 12 xs) ||
    (all (>0) $ drop 8 $ xs) ||
    (all (>0) $ (last xs) : (take 4 xs))
