module ZerolessQuaternaryRandomAccessList (QuaternaryList) where

import Prelude hiding (head, tail, lookup)
import RandomAccessList

data Tree a = Leaf a | Node (Vector a) deriving Show
type Vector a = [Tree a]
newtype QuaternaryList a = QL [Vector a] deriving Show

consTree :: Tree a -> [Vector a] -> [Vector a]
consTree t [] = [[t]]
consTree t (ts : ds)
  | length ts < 4 = (t : ts) : ds
  | otherwise = [t] : consTree (Node ts) ds

unconsTree :: [Vector a] -> (Tree a, [Vector a])
unconsTree [] = error "empty list"
unconsTree [[t]] = (t, [])
unconsTree ([t] : ds) = (t, t' : ds')
  where (Node t', ds') = unconsTree ds
unconsTree ((t : ts) : ds) = (t, ts : ds)

lookupTree :: Int -> Int -> Tree a -> a
lookupTree 1 0 (Leaf x) = x
lookupTree _ _ (Leaf _) = error "bad subscript"
lookupTree w i (Node ts) = look w i ts
  where
  look w i (t : ts)
    | i < w' = lookupTree w' i t
    | otherwise = look w (i - w') ts
    where w' = w `div` 4

updateTree :: Int -> Int -> a -> Tree a -> Tree a
updateTree 1 0 y (Leaf _) = Leaf y
updateTree _ _ _ (Leaf _) = error "bad subscript"
updateTree w i y (Node ts) = Node (upd w i y ts)
  where
  upd w i y (t : ts)
    | i < w' = updateTree w' i y t : ts
    | otherwise = t : upd w (i - w') y ts
    where w' = w `div` 4

instance RandomAccessList QuaternaryList where

  empty = QL []

  isEmpty (QL ds) = null ds

  cons x (QL ds) = QL (consTree (Leaf x) ds)

  head (QL ds) = x
    where (Leaf x, _) = unconsTree ds
  
  tail (QL ds) = QL ds'
    where (_, ds') = unconsTree ds
  
  lookup i (QL ds) = look 1 i ds
    where
    look _ _ [] = error "bad subscript"
    look w i ([] : ds) = look (w * 4) i ds
    look w i ((t : ts) : ds)
      | i < w = lookupTree w i t
      | otherwise = look w (i - w) (ts : ds)

  update i y (QL ds) = QL (upd 1 i y ds)
    where
    upd _ _ _ [] = error "bad subscript"
    upd w i y ([] : ds) = [] : upd (w * 4) i y ds
    upd w i y ((t : ts) : ds)
      | i < w = (updateTree w i y t : ts) : ds
      | otherwise = (t : ts') : ds'
      where (ts' : ds') = upd w (i - w) y (ts : ds)
