module TrinomialHeap (TrinomialHeap) where

import Heap
import Data.List (sortBy)
import Data.Function (on)

data Tree a = Node a [(Tree a, Tree a)] deriving Show
data Digit a = Zero | One (Tree a) | Two (Tree a) (Tree a) deriving Show
newtype TrinomialHeap a = TH [Digit a] deriving Show

root :: Tree a -> a
root (Node x _) = x

link :: Ord a => Tree a -> Tree a -> Tree a -> Tree a
link t1 t2 t3 = Node x1 ((t2', t3') : ts1)
  where [Node x1 ts1, t2', t3'] = sortBy (compare `on` root) [t1, t2, t3]

insTree :: Ord a => Tree a -> [Digit a] -> [Digit a]
insTree t [] = [One t]
insTree t (Zero : ts) = One t : ts
insTree t (One t1 : ts) = Two t t1 : ts
insTree t (Two t1 t2 : ts) = Zero : insTree (link t t1 t2) ts

mrg :: Ord a => [Digit a] -> [Digit a] -> [Digit a]
mrg ds1 [] = ds1
mrg [] ds1 = ds1
mrg (Zero : ds1) (d1 : ds2) = d1 : mrg ds1 ds2
mrg (d1 : ds1) (Zero : ds2) = d1 : mrg ds1 ds2
mrg (One t1 : ds1) (One t2 : ds2) = Two t1 t2 : mrg ds1 ds2
mrg (One t1 : ds1) (Two t2 t3 : ds2) = Zero : insTree (link t1 t2 t3) (mrg ds1 ds2)
mrg (Two t1 t2 : ds1) (One t3 : ds2) = Zero : insTree (link t1 t2 t3) (mrg ds1 ds2)
mrg (Two t1 t2 : ds1) (Two t3 t4 : ds2) = One t1 : insTree (link t2 t3 t4) (mrg ds1 ds2)

removeMinTree :: Ord a => [Digit a] -> (Tree a, [Digit a])
removeMinTree [] = error "empty heap"
removeMinTree [One t] = (t, [])
removeMinTree [Two t1 t2]
  | root t1 <= root t2 = (t1, [One t2])
  | otherwise = (t2, [One t1])
removeMinTree (One t : ds)
  | root t <= root t' = (t, Zero : ds)
  where (t', _) = removeMinTree ds
removeMinTree (Two t1 t2 : ds)
  | root t1 <= root t' && root t1 <= root t2 = (t1, One t2 : ds)
  | root t2 <= root t' && root t2 <= root t1 = (t2, One t1 : ds)
  where (t', _) = removeMinTree ds
removeMinTree (d : ds) = (t', d : ds')
  where (t', ds') = removeMinTree ds

instance Heap TrinomialHeap where

  empty = TH []

  isEmpty (TH ts) = null ts

  insert x (TH ds) = TH (insTree (Node x []) ds)

  merge (TH ds1) (TH ds2) = TH (mrg ds1 ds2)

  findMin (TH ts) = root t
    where (t, _) = removeMinTree ts

  deleteMin (TH ts) = TH (mrg (reverse (map (uncurry Two) ts1)) ts2)
    where (Node _ ts1, ts2) = removeMinTree ts
