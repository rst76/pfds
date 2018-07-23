module SimpleQueue where
import Queue

newtype SimpleQueue a = SQ [a] deriving Show

instance Queue SimpleQueue where

  empty = SQ []

  isEmpty (SQ []) = True
  isEmpty _ = False

  snoc (SQ xs) x = SQ (xs ++ [x])

  head (SQ []) = error "empty queue"
  head (SQ (x : _)) = x

  tail (SQ []) = error "empty queue"
  tail (SQ (_ : xs)) = SQ xs
