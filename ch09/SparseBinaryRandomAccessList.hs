module SparseBinaryRandomAccessList (BinaryList) where

import Prelude hiding (head, tail, lookup)
import RandomAccessList

data Tree a = Leaf a | Node Int (Tree a) (Tree a) deriving Show
newtype BinaryList a = BL [Tree a] deriving Show

size :: Tree a -> Int
size (Leaf _) = 1
size (Node w t1 t2) = w

link :: Tree a -> Tree a -> Tree a
link t1 t2 = Node (size t1 + size t2) t1 t2

consTree :: Tree a -> [Tree a] -> [Tree a]
consTree t [] = [t]
consTree t1 (t2 : ts)
  | size t1 < size t2 = t1 : t2 : ts
  | otherwise = consTree (link t1 t2) ts

unconsTree :: [Tree a] -> (Tree a, [Tree a])
unconsTree [] = error "empty list"
unconsTree (Leaf x : ts) = (Leaf x, ts)
unconsTree (Node _ t1 t2 : ts) = unconsTree (t1 : t2 : ts)

lookupTree :: Int -> Tree a -> a
lookupTree 0 (Leaf x) = x
lookupTree _ (Leaf _) = error "bad subscript"
lookupTree i (Node w t1 t2)
  | i < w `div` 2 = lookupTree i t1
  | otherwise = lookupTree (i - w `div` 2) t2

look :: Int -> [Tree a] -> a
look _ [] = error "bad subscript"
look i (t : ts)
  | i < size t = lookupTree i t
  | otherwise = look (i - size t) ts

updateTree :: Int -> a -> Tree a -> Tree a
updateTree 0 y (Leaf _) = Leaf y
updateTree i _ (Leaf _) = error "bad subscript"
updateTree i y (Node w t1 t2)
  | i < w `div` 2 = Node w (updateTree i y t1) t2
  | otherwise = Node w t1 (updateTree (i - w `div` 2) y t2)

upd :: Int -> a -> [Tree a] -> [Tree a]
upd _ _ [] = error "bad subscript"
upd i y (t : ts)
  | i < size t = updateTree i y t : ts
  | otherwise = t : upd (i - size t) y ts

instance RandomAccessList BinaryList where

  empty = BL []
  
  isEmpty (BL ts) = null ts

  cons x (BL ts) = BL (consTree (Leaf x) ts)

  head (BL ts) = let (Leaf x, _) = unconsTree ts in x

  tail (BL ts) = let (_, ts') = unconsTree ts in BL ts'

  lookup i (BL ts) = look i ts

  update i y (BL ts) = BL (upd i y ts)
