module SkewBinaryRandomAccessList (SkewList) where

import Prelude hiding (head, tail, lookup)
import RandomAccessList

data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving Show
newtype SkewList a = SL [(Int, Tree a)] deriving Show

instance RandomAccessList SkewList where

  empty = SL []

  isEmpty (SL ts) = null ts

  cons x (SL ((w1, t1) : (w2, t2) : ts))
    | w1 == w2 = SL ((1 + w1 + w2, Node x t1 t2) : ts)
  cons x (SL ts) = SL ((1, Leaf x) : ts)

  head (SL []) = error "empty list"
  head (SL ((_, Leaf x) : _)) = x
  head (SL ((_, Node x _ _) : _)) = x

  tail (SL []) = error "empty list"
  tail (SL ((1, _) : ts)) = SL ts
  tail (SL ((w, Node x t1 t2) : ts)) = SL ((w `div` 2, t1) : (w `div` 2, t2) : ts)

  lookup i (SL ts) = look i ts
    where
    look i [] = error "bad subscript"
    look i ((w, t) : ts) =
      if i < w then lookTree w i t else look (i - w) ts
    lookTree 1 0 (Leaf x) = x
    lookTree 1 _ (Leaf _) = error "bad subscript"
    lookTree w 0 (Node x _ _) = x
    lookTree w i (Node _ t1 t2) =
      if i <= w' then lookTree w' (i - 1) t1 else lookTree w' (i - 1 - w') t2
      where w' = w `div` 2

  update i y (SL ts) = SL (upd i ts)
    where
    upd i [] = error "empty list"
    upd i ((w, t) : ts) =
      if i < w then (w, updTree w i t) : ts else (w, t) : upd (i - w) ts
    updTree 1 0 (Leaf _) = Leaf y
    updTree 1 _ (Leaf _) = error "bad subscript"
    updTree w 0 (Node _ t1 t2) = Node y t1 t2
    updTree w i (Node x t1 t2) =
      if i <= w' then Node x (updTree w' (i - 1) t1) t2
      else Node x t1 (updTree w' (i - 1 - w') t2)
      where w' = w `div` 2
