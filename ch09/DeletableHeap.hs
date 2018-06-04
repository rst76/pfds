module DeletableHeap (DeletableHeap, delete) where

import qualified Heap as H

data DeletableHeap h a = DH (h a) (h a) deriving Show

normalize :: Ord a => H.Heap h => DeletableHeap h a -> DeletableHeap h a
normalize (DH p n)
  | H.isEmpty p || H.isEmpty n || H.findMin p /= H.findMin n = DH p n
  | otherwise = normalize (DH (H.deleteMin p) (H.deleteMin n))

delete :: Ord a => H.Heap h => a -> DeletableHeap h a -> DeletableHeap h a
delete x (DH p n) = normalize (DH p (H.insert x n))

instance H.Heap h => H.Heap (DeletableHeap h) where

  empty = DH H.empty H.empty

  isEmpty (DH p n) = H.isEmpty p

  insert x (DH p n) = normalize (DH (H.insert x p) n)

  merge (DH p1 n1) (DH p2 n2) = normalize (DH (H.merge p1 p2) (H.merge n1 n2))

  findMin (DH p _) = H.findMin p

  deleteMin (DH p n) = normalize (DH (H.deleteMin p) n)
