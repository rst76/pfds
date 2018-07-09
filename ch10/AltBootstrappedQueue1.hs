module AltBootstrappedQueue1 (BootstrappedQueue) where

import Prelude hiding (head, tail)
import Queue

data Elem a = Elem a | List [Elem a] deriving Show
data BootstrappedQueue a =
  E | Q Int [Elem a] (BootstrappedQueue a) Int [Elem a] deriving Show

headL :: BootstrappedQueue a -> [Elem a]
headL (Q _ (List x : _) _ _ _) = x

snocL :: BootstrappedQueue a -> [Elem a] -> BootstrappedQueue a
snocL E x = Q 1 [List x] E 0 []
snocL (Q lenfm f m lenr r) x = checkQ lenfm f m (lenr + 1) (List x : r)

checkQ, checkF ::
  Int -> [Elem a] -> (BootstrappedQueue a) -> Int -> [Elem a] -> BootstrappedQueue a

checkQ lenfm f m lenr r
  | lenr <= lenfm = checkF lenfm f m lenr r
  | otherwise = checkF (lenfm + lenr) f (snocL m (reverse r)) 0 []

checkF _ [] E _ _ = E
checkF lenfm [] m lenr r = Q lenfm (headL m) (tail m) lenr r
checkF lenfm f  m lenr r = Q lenfm f m lenr r

instance Queue BootstrappedQueue where

  empty = Q 0 [] E 0 []

  isEmpty E = True
  isEmpty _ = False

  snoc E x = Q 1 [Elem x] E 0 []
  snoc (Q lenfm f m lenr r) x = checkQ lenfm f m (lenr + 1) (Elem x : r)

  head E = error "empty queue"
  head (Q _ (Elem x : _) _ _ _) = x

  tail E = error "empty queue"
  tail (Q lenfm (_ : f') m lenr r) = checkQ (lenfm - 1) f' m lenr r
