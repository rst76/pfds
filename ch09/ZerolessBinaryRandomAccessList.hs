module ZerolessBinaryRandomAccessList (BinaryList) where

import Prelude hiding (head, tail, lookup)
import RandomAccessList

data Tree a = Leaf a | Node Int (Tree a) (Tree a) deriving Show
data Digit a = One (Tree a) | Two (Tree a) (Tree a) deriving Show
newtype BinaryList  a = BL [Digit a] deriving Show

size :: Tree a -> Int
size (Leaf _) = 1
size (Node w _ _) = w

link :: Tree a -> Tree a -> Tree a
link t1 t2 = Node (size t1 + size t2) t1 t2

consTree :: Tree a -> [Digit a] -> [Digit a]
consTree t [] = [One t]
consTree t (One t1 : ds) = Two t t1 : ds
consTree t (Two t1 t2 : ds) = One t : consTree (link t1 t2) ds

unconsTree :: [Digit a] -> (Tree a, [Digit a])
unconsTree [] = error "empty list"
unconsTree [One t] = (t, [])
unconsTree (Two t1 t2 : ds) = (t1, One t2 : ds)
unconsTree (One t : ds) =  (t, Two t1 t2 : ds')
  where (Node _ t1 t2, ds') = unconsTree ds

lookupTree :: Int -> Tree a -> a
lookupTree 0 (Leaf x) = x
lookupTree _ (Leaf _) = error "bad subscript"
lookupTree i (Node w t1 t2)
  | i < w `div` 2 = lookupTree i t1
  | otherwise = lookupTree (i - w `div` 2) t2

look :: Int -> [Digit a] -> a
look _ [] = error "bad subscript"
look i (One t : ds)
  | i < size t = lookupTree i t
  | otherwise = look (i - size t) ds
look i (Two t1 t2 : ds)
  | i < size t1 = lookupTree i t1
  | i < size t1 + size t2 = lookupTree i t2
  | otherwise = look (i - size t1 - size t2) ds

updateTree :: Int -> a -> Tree a -> Tree a
updateTree 0 y (Leaf _) = Leaf y
updateTree i _ (Leaf _) = error "bad subscript"
updateTree i y (Node w t1 t2)
  | i < w `div` 2 = Node w (updateTree i y t1) t2
  | otherwise = Node w t1 (updateTree (i - w `div` 2) y t2)

upd :: Int -> a -> [Digit a] -> [Digit a]
upd _ _ [] = error "bad subscript"
upd i y (One t : ds)
  | i < size t = One (updateTree i y t) : ds
  | otherwise = One t : upd (i - size t) y ds
upd i y (Two t1 t2 : ds)
  | i < size t1 = Two (updateTree i y t1) t2 : ds
  | i < size t1 + size t2 = Two t1 (updateTree i y t2) : ds
  | otherwise = Two t1 t2 : upd (i - size t1 - size t2) y ds

instance RandomAccessList BinaryList  where

  empty = BL []
  
  isEmpty (BL ds) = null ds

  cons x (BL ds) = BL (consTree (Leaf x) ds)

  head (BL ds) = x
    where (Leaf x, _) = unconsTree ds

  tail (BL ds) = BL ds'
    where (_, ds') = unconsTree ds

  lookup i (BL ds) = look i ds

  update i y (BL ds) = BL (upd i y ds)
