module ExplicitMinHeap (ExplicitMinHeap) where

import qualified Heap as H

data ExplicitMinHeap h a = E | NE a (h a) deriving Show

instance H.Heap h => H.Heap (ExplicitMinHeap h) where

  empty = E

  isEmpty E = True
  isEmpty _ = False

  insert x E = NE x (H.insert x H.empty)
  insert x (NE y h) = NE (min x y) (H.insert x h)

  merge h E = h
  merge E h = h
  merge (NE x h1) (NE y h2) = NE (min x y) (H.merge h1 h2)

  findMin E = error "empty heap"
  findMin (NE x _) = x

  deleteMin E = error "empty heap"
  deleteMin (NE x h)
    | H.isEmpty h' = E
    | otherwise    = NE y h'
    where
    h' = H.deleteMin h
    y  = H.findMin h'
