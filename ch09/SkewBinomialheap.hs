module SkewBinomialHeap (SkewBinomialHeap) where

import Heap

data Tree a = Node Int a [a] [Tree a] deriving Show

newtype SkewBinomialHeap a = SBH [Tree a] deriving Show

rank :: Tree a -> Int
rank (Node r _ _ _) = r

root :: Tree a -> a
root (Node _ x _ _) = x

link :: Ord a => Tree a -> Tree a -> Tree a
link t1@(Node r x1 xs1 c1) t2@(Node _ x2 xs2 c2) =
  if x1 <= x2 then Node (r + 1) x1 xs1 (t2 : c1)
  else Node (r + 1) x2 xs2 (t1 : c2)

skewLink :: Ord a => a -> Tree a -> Tree a -> Tree a
skewLink x t1 t2 =
  let Node r y ys c = link t1 t2
  in if x <= y then Node r x (y : ys) c else Node r y (x : ys) c

insTree :: Ord a => Tree a -> [Tree a] -> [Tree a]
insTree t [] = [t]
insTree t ts@(t' : ts') =
  if rank t < rank t' then t : ts else insTree (link t t') ts'

mrg :: Ord a => [Tree a] -> [Tree a] -> [Tree a]
mrg ts1 [] = ts1
mrg [] ts2 = ts2
mrg ts1@(t1 : ts1') ts2@(t2 : ts2')
  | rank t1 < rank t2 = t1 : mrg ts1' ts2
  | rank t2 < rank t1 = t2 : mrg ts1 ts2'
  | otherwise = insTree (link t1 t2) (mrg ts1' ts2')

normalize :: Ord a => [Tree a] -> [Tree a]
normalize (t : ts) = insTree t ts

removeMinTree :: Ord a => [Tree a] -> (Tree a, [Tree a])
removeMinTree [t] = (t, [])
removeMinTree (t : ts) = if root t < root t' then (t, ts) else (t', t : ts')
  where (t', ts') = removeMinTree ts

instance Heap SkewBinomialHeap where

  empty = SBH []
  isEmpty (SBH ts) = null ts

  insert x (SBH (t1 : t2 : ts))
    | rank t1 == rank t2 = SBH (skewLink x t1 t2 : ts)
  insert x (SBH ts) = SBH (Node 0 x [] [] : ts)

  merge (SBH ts1) (SBH ts2) = SBH (mrg (normalize ts1) (normalize ts2))

  findMin (SBH ts) = root t
    where (t, _) = removeMinTree ts

  deleteMin (SBH ts) = foldr insert (SBH ts') xs
    where
    (Node _ x xs ts1, ts2) = removeMinTree ts
    ts' = mrg (reverse ts1) (reverse ts2)
