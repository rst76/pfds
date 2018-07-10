{-# LANGUAGE StandaloneDeriving, UndecidableInstances #-}

module AltBootstrappedQueue2 (BootstrappedQueue) where

import qualified Queue as PQ

data BootstrappedQueue q a = E | Q Int [a] (q [a]) Int [a]
deriving instance (Show a, Show (q [a])) => Show (BootstrappedQueue q a)

checkQ, checkF :: PQ.Queue q =>
  Int -> [a] -> q [a] -> Int -> [a] -> BootstrappedQueue q a

checkQ lenfm f m lenr r
  | lenr <= lenfm = checkF lenfm f m lenr r
  | otherwise = checkF (lenfm + lenr) f (PQ.snoc m (reverse r)) 0 []

checkF lenfm [] m lenr r
  | PQ.isEmpty m = E
  | otherwise = Q lenfm (PQ.head m) (PQ.tail m) lenr r
checkF lenfm f  m lenr r = Q lenfm f m lenr r

instance PQ.Queue q => PQ.Queue (BootstrappedQueue q) where

  empty = Q 0 [] PQ.empty 0 []

  isEmpty E = True
  isEmpty _ = False

  snoc E x = Q 1 [x] PQ.empty 0 []
  snoc (Q lenfm f m lenr r) x = checkQ lenfm f m (lenr + 1) (x : r)

  head E = error "empty queue"
  head (Q _ (x : _) _ _ _) = x

  tail E = error "empty queue"
  tail (Q lenfm (_ : f') m lenr r) = checkQ (lenfm - 1) f' m lenr r
