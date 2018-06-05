module SegmentedBinaryRandomAccessList (BinaryList) where

import Prelude hiding (head, tail, lookup)
import RandomAccessList

data Tree a = Leaf a | Node Int (Tree a) (Tree a) deriving Show
data Digit a = Zero
  | Ones [Tree a]
  | Two (Tree a) (Tree a)
  | Threes [(Tree a, Tree a, Tree a)]
  | Four (Tree a) (Tree a) (Tree a) (Tree a)
  deriving Show
newtype BinaryList a = BL [Digit a]

size :: Tree a -> Int
size (Leaf _) = 1
size (Node w _ _) = w

link :: Tree a -> Tree a -> Tree a
link t1 t2 = Node (size t1 + size t2) t1 t2

ones :: [Tree a] -> [Digit a] -> [Digit a]
ones [] ds = ds
ones ts (Ones ts1 : ds) = Ones (ts ++ ts1) : ds
ones ts ds = Ones ts : ds

threes :: [(Tree a, Tree a, Tree a)] -> [Digit a] -> [Digit a]
threes [] ds = ds
threes ts (Threes ts1 : ds) = Threes (ts ++ ts1) : ds
threes ts ds = Threes ts : ds

fixup :: [Digit a] -> [Digit a]
fixup (Threes ts : ds) = Threes ts : fixup ds
fixup (Four t1 t2 t3 t4 : ds) = Two t1 t2 : consTree (link t3 t4) ds
fixup ds = ds

consTree :: Tree a -> [Digit a] -> [Digit a]
consTree t [] = [Ones [t]]
consTree t (Zero : ds) = ones [t] ds
consTree t (Ones (t1 : ts) : ds) = Two t t1 : ones ts ds
consTree t (Two t1 t2 : ds) = threes [(t, t1, t2)] ds
consTree t (Threes ((t1, t2, t3) : ts) : ds) = Four t t1 t2 t3 : threes ts ds

fixdown :: [Digit a] -> [Digit a]
fixdown (Ones ts : ds) = Ones ts : fixdown ds
fixdown (Zero : ds) = Two t1 t2 : ds'
  where (Node _ t1 t2, ds') = unconsTree ds
fixdown ds = ds

unconsTree :: [Digit a] -> (Tree a, [Digit a])
unconsTree [] = error "empty list"
unconsTree [Ones [t]] = (t, [])
unconsTree (Ones (t : ts) : ds) = (t, Zero : ones ts ds)
unconsTree (Two t t1 : ts) = (t, ones [t1] ts)
unconsTree (Threes ((t, t1, t2) : ts) : ds) = (t, Two t1 t2 : threes ts ds)
unconsTree (Four t t1 t2 t3 : ds) =  (t, threes [(t1, t2, t3)] ds)

lookupTree :: Int -> Tree a -> a
lookupTree 0 (Leaf x) = x
lookupTree _ (Leaf _) = error "bad subscript"
lookupTree i (Node w t1 t2)
  | i < w `div` 2 = lookupTree i t1
  | otherwise = lookupTree (i - w `div` 2) t2

look :: Int -> [Digit a] -> a
look _ [] = error "bad subscript"
look i (Zero : ds) = look i ds
look i (Ones (t : ts) : ds)
  | i < size t = lookupTree i t
  | otherwise = look (i - size t) (ones ts ds)
look i (Two t t1 : ds)
  | i < size t = lookupTree i t
  | otherwise = look (i - size t) (ones [t1] ds)
look i (Threes ((t, t1, t2) : ts) : ds)
  | i < size t = lookupTree i t
  | otherwise = look (i - size t) (Two t1 t2 : threes ts ds)
look i (Four t t1 t2 t3 : ds)
  | i < size t = lookupTree i t
  | otherwise = look (i - size t) (threes [(t1, t2, t3)] ds)

instance RandomAccessList BinaryList where

  empty = BL []

  isEmpty (BL ds) = null ds

  cons x (BL ds) = BL (fixup (consTree (Leaf x) ds))

  head (BL ds) = x
    where (Leaf x, _) = unconsTree ds
  
  tail (BL ds) = BL (fixdown ds')
    where (_, ds') = unconsTree ds
  
  lookup i (BL ds) = look i ds

  update i y (BL ds) = undefined
