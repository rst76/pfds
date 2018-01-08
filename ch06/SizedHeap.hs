module SizedHeap (SizedHeap) where

import qualified Heap as H

data SizedHeap h a = SH Int (h a) deriving Show

instance H.Heap h => H.Heap (SizedHeap h) where

  empty = SH 0 H.empty

  isEmpty (SH s _) = s == 0

  insert x (SH s h) = SH (s + 1) (H.insert x h)

  merge (SH s1 h1) (SH s2 h2) = SH (s1 + s2) (H.merge h1 h2)

  findMin (SH _ h) = H.findMin h

  deleteMin (SH s h) = SH (s - 1) (H.deleteMin h)
