module OutputRestrictedDeque where

import qualified Queue as Q

data OutputRestrictedDeque q a = ORD [a] (q a) deriving Show

cons :: a -> OutputRestrictedDeque q a -> OutputRestrictedDeque q a
cons x (ORD f q) = ORD (x : f) q

instance Q.Queue q => Q.Queue (OutputRestrictedDeque q) where

  empty = ORD [] Q.empty

  isEmpty (ORD [] q) = Q.isEmpty q
  isEmpty _ = False

  snoc (ORD f q) x = ORD f (Q.snoc q x)

  head (ORD (x : _) _) = x
  head (ORD _ q) = Q.head q

  tail (ORD (_ : f) q) = ORD f q
  tail (ORD _ q) = ORD [] (Q.tail q)
