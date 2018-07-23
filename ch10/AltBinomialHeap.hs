module AltBinomialHeap (BinomialHeap) where

import Heap

data Tree a = Node Int (BinomialHeap a) [Tree a] deriving Show
data BinomialHeap a = E | NE a [Tree a] deriving Show

rank :: Tree a -> Int
rank (Node r _ _) = r

root :: Tree a -> a
root (Node _ (NE x _) _) = x

tree :: a -> Tree a
tree x = Node 0 (NE x []) []

link :: Ord a => Tree a -> Tree a -> Tree a
link t1@(Node r h1 c1) t2@(Node _ h2 c2)
  | root t1 < root t2 = Node (r+1) h1 (t2 : c1)
  | otherwise = Node (r+1) h2 (t1 : c2)

insTree :: Ord a => Tree a -> [Tree a] -> [Tree a]
insTree t [] = [t]
insTree t ts@(t' : ts')
  | rank t < rank t' = t : ts
  | otherwise = insTree (link t t') ts'

mrg :: Ord a => [Tree a] -> [Tree a] -> [Tree a]
mrg ts1 [] = ts1
mrg [] ts2 = ts2
mrg ts1@(t1 : ts1') ts2@(t2 : ts2')
  | rank t1 < rank t2 = t1 : mrg ts1' ts2
  | rank t2 < rank t1 = t2 : mrg ts1 ts2'
  | otherwise = insTree (link t1 t2) (mrg ts1' ts2')

removeMinTree :: Ord a => [Tree a] -> (Tree a, [Tree a])
removeMinTree [] = error "empty heap"
removeMinTree [t] = (t, [])
removeMinTree (t : ts)
  | root t < root t' = (t, ts)
  | otherwise = (t', t : ts')
  where (t', ts') = removeMinTree ts

instance Heap BinomialHeap where

  empty = E

  isEmpty E = True
  isEmpty _ = False

  insert x E = NE x []
  insert x (NE y ts)
    | x <= y    = NE x (insTree (tree y) ts)
    | otherwise = NE y (insTree (tree x) ts)

  merge E h = h
  merge h E = h
  merge h1@(NE x ts1) h2@(NE y ts2)
    | x <= y    = NE x (insTree (tree y) (mrg ts1 ts2))
    | otherwise = NE y (insTree (tree x) (mrg ts1 ts2))

  findMin E = error "empty heap"
  findMin (NE x _) = x

  deleteMin E = error "empty heap"
  deleteMin (NE _ ts) = NE x (mrg (reverse ts1) ts2)
    where (Node _ (NE x []) ts1, ts2) = removeMinTree ts
