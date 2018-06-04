module SkewTrinaryRandomAccessList (SkewList) where

import Prelude hiding (head, tail, lookup)
import RandomAccessList

data Tree a = Leaf a | Node a (Tree a) (Tree a) (Tree a) deriving Show
newtype SkewList a = SL [(Int, Tree a)] deriving Show

lookupTree :: Int -> Int -> Tree a -> a
lookupTree 1 0 (Leaf x) = x
lookupTree 1 _ (Leaf _) = error "bad subscript"
lookupTree w 0 (Node x _ _ _) = x
lookupTree w i (Node _ t1 t2 t3)
  | i <= w'     = lookupTree w' (i - 1) t1
  | i <= w' * 2 = lookupTree w' (i - 1 - w') t2
  | otherwise   = lookupTree w' (i - 1 - w' * 2) t3
  where w' = w `div` 3

updateTree :: Int -> Int -> a -> Tree a -> Tree a
updateTree 1 0 y (Leaf _) = Leaf y
updateTree 1 _ y (Leaf _) = error "bad subscript"
updateTree w 0 y (Node _ t1 t2 t3) = Node y t1 t2 t3
updateTree w i y (Node x t1 t2 t3)
  | i <= w'     = Node x (updateTree w' (i - 1) y t1) t2 t3
  | i <= w' * 2 = Node x t1 (updateTree w' (i - 1 - w') y t2) t3
  | otherwise   = Node x t1 t2 (updateTree w' (i - 1 - w' * 2) y t3)
  where w' = w `div` 3
  
instance RandomAccessList SkewList where

  empty = SL []

  isEmpty (SL ts) = null ts

  cons x (SL ((w1, t1) : (w2, t2) : (w3, t3) : ts))
    | w1 == w2 && w2 == w3 = SL ((1 + w1 + w2 + w3, Node x t1 t2 t3) : ts)
  cons x (SL ts) = SL ((1, Leaf x) : ts)

  head (SL []) = error "empty list"
  head (SL ((_, Leaf x) : _)) = x
  head (SL ((_, Node x _ _ _) : _)) = x

  tail (SL []) = error "empty list"
  tail (SL ((1, _) : ts)) = SL ts
  tail (SL ((w, Node x t1 t2 t3) : ts)) = SL ((w', t1) : (w', t2) : (w', t3) : ts)
    where w' = w `div` 3

  lookup i (SL ts) = look i ts
    where
    look i [] = error "bad subscript"
    look i ((w, t) : ts) =
      if i < w then lookupTree w i t else look (i - w) ts

  update i y (SL ts) = SL (upd i ts)
    where
    upd i [] = error "empty list"
    upd i ((w, t) : ts) =
      if i < w then (w, updateTree w i y t) : ts else (w, t) : upd (i - w) ts
