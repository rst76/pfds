module BootstrappedQueue (BootstrappedQueue) where

import Prelude hiding (head, tail)
import Queue

data BootstrappedQueue a =
  E | Q Int [a] (BootstrappedQueue [a]) Int [a] deriving Show

checkQ, checkF ::
  Int -> [a] -> BootstrappedQueue [a] -> Int -> [a] -> BootstrappedQueue a

checkQ lenfm f m lenr r
  | lenr <= lenfm = checkF lenfm f m lenr r
  | otherwise = checkF (lenfm + lenr) f (snoc m (reverse r)) 0 []

checkF lenfm [] E lenr r = E
checkF lenfm [] m lenr r = Q lenfm (head m) (tail m) lenr r
checkF lenfm f  m lenr r = Q lenfm f m lenr r

instance Queue BootstrappedQueue where

  empty = Q 0 [] E 0 []

  isEmpty E = True
  isEmpty _ = False

  snoc E x = Q 1 [x] E 0 []
  snoc (Q lenfm f m lenr r) x = checkQ lenfm f m (lenr + 1) (x : r)

  head E = error "empty queue"
  head (Q _ (x : _) _ _ _) = x

  tail E = error "empty queue"
  tail (Q lenfm (_ : f') m lenr r) = checkQ (lenfm - 1) f' m lenr r
