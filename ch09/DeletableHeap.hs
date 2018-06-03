module DeletableHeap(DeletableHeap, delete) where

import qualified Heap as H

data DeletableHeap h a = DH (h a) (h a) deriving Show

normalize :: Ord a => H.Heap h => DeletableHeap h a -> DeletableHeap h a
normalize (DH n p)
  | H.isEmpty n || H.isEmpty p || H.findMin n /= H.findMin p = DH n p
  | otherwise = normalize (DH (H.deleteMin n) (H.deleteMin p))

delete :: Ord a => H.Heap h => a -> DeletableHeap h a -> DeletableHeap h a
delete x (DH n p) = normalize (DH (H.insert x n) p)

instance H.Heap h => H.Heap (DeletableHeap h) where

  empty = DH H.empty H.empty

  isEmpty (DH n p) = H.isEmpty p

  insert x (DH n p) = normalize (DH n (H.insert x p))

  merge (DH n1 p1) (DH n2 p2) = normalize (DH (H.merge n1 n2) (H.merge p1 p2))

  findMin (DH _ p) = H.findMin p

  deleteMin (DH n p) = normalize (DH n (H.deleteMin p))
