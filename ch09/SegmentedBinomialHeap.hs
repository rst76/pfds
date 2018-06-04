module SegmentedBinomialHeap where

import Prelude hiding (head, tail, lookup)
import Heap

data Tree a = Node a [Tree a] deriving Show
data Digit a = Zero | Ones [Tree a] | Two (Tree a) (Tree a) deriving Show
newtype BinomialHeap a = BH [Digit a] deriving Show

link :: Ord a => Tree a -> Tree a -> Tree a
link t1@(Node x1 c1) t2@(Node x2 c2)
  | x1 <= x2  = Node x1 (t2 : c1)
  | otherwise = Node x2 (t1 : c2)

ones :: [Tree a] -> [Digit a] -> [Digit a]
ones [] ds = ds
ones ts (Ones ts1 : ds) = Ones (ts ++ ts1) : ds
ones ts ds = Ones ts : ds

fixup :: Ord a => [Digit a] -> [Digit a]
fixup (Ones ts : ds) = Ones ts : fixup ds
fixup (Two t1 t2 : ds) = Zero : simpleInc (link t1 t2) ds
fixup ds = ds

simpleInc :: Tree a -> [Digit a] -> [Digit a]
simpleInc t [] = [Ones [t]]
simpleInc t (Zero : ds) = ones [t]  ds
simpleInc t (Ones (t1 : ts) : ds) = Two t t1 : ones ts ds

insTree :: Ord a => Tree a -> [Digit a] -> [Digit a]
insTree t ds = fixup (simpleInc t ds)

mrg :: Ord a => [Digit a] -> [Digit a] -> [Digit a]
mrg ds1 [] = ds1
mrg [] ds2 = ds2
mrg (Zero : ds1) (Zero : ds2) = Zero : mrg ds1 ds2
mrg (Zero : ds1) (Ones (t : ts) : ds2) = ones [t] (mrg ds1 (ones ts ds2))
mrg (Ones (t : ts) : ds1) (Zero : ds2) = ones [t] (mrg (ones ts ds1) ds2)
mrg (Ones (t1 : ts1) : ds1) (Ones (t2 : ts2) : ds2) =
  Zero : insTree (link t1 t2) (mrg (ones ts1 ds1) (ones ts2 ds2))
mrg (Zero : ds1) (Two t1 t2 : ds2) = Two t1 t2 : mrg ds1 ds2
mrg (Two t1 t2 : ds1) (Zero : ds2) = Two t1 t2 : mrg ds1 ds2
mrg (Ones (t : ts) : ds1) (Two t1 t2 : ds2) =
  ones [t] (insTree (link t1 t2) (mrg (ones ts ds1) ds2))
mrg (Two t1 t2 : ds1) (Ones (t : ts) : ds2) =
  ones [t] (insTree (link t1 t2) (mrg ds1 (ones ts ds2)))
mrg (Two t1 t2 : ds1) (Two t3 t4 : ds2) =
  Two t1 t2 : insTree (link t3 t4) (mrg ds1 ds2)

removeMinTree :: Ord a => [Digit a] -> (Tree a, [Digit a])
removeMinTree [] = error "empty heap"
removeMinTree [Ones [t]] = (t, [])
removeMinTree [Two t1@(Node x1 _) t2@(Node x2 _)]
  | x1 <= x2 = (t1, [Ones [t2]])
  | otherwise = (t2, [Ones [t1]])
removeMinTree (Zero : ds) = (t', Zero : ds')
  where (t', ds') = removeMinTree ds
removeMinTree (Ones (t@(Node x _) : ts) : ds)
  | x <= x' = (t, Zero : ones ts ds)
  | otherwise = (t', ones [t] ds')
  where (t'@(Node x' _), ds') = removeMinTree (ones ts ds)
removeMinTree (Two t1@(Node x1 _) t2@(Node x2 _) : ds)
  | x1 <= x2 && x1 <= x' = (t1, ones [t2] ds)
  | x2 <= x1 && x2 <= x' = (t2, ones [t1] ds)
  | otherwise = (t', Two t1 t2 : ds')
  where (t'@(Node x' _), ds') = removeMinTree ds

instance Heap BinomialHeap where

  empty = BH []

  isEmpty (BH ds) = null ds

  insert x (BH ds) = BH (insTree (Node x []) ds)

  merge (BH ds1) (BH ds2) = BH (mrg ds1 ds2)

  findMin (BH ds) = x
    where (Node x _, _) = removeMinTree ds
  
  deleteMin (BH ds) = BH (mrg (ones (reverse ts') []) ds')
    where (Node _ ts', ds') = removeMinTree ds
  