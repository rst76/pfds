module AltBinaryRandomAccessList (BinaryList) where

import Prelude hiding (head, tail, lookup)
import RandomAccessList

data BinaryList a = Nil
  | One a (BinaryList (a, a))
  | Two a a (BinaryList (a, a))
  | Three a a a (BinaryList (a, a)) deriving Show

uncons:: BinaryList a -> (a, BinaryList a)
uncons Nil = error "empty list "
uncons (One x Nil) = (x, Nil)
uncons (Two t t1 ds) = (t, One t1 ds)
uncons (Three t t1 t2 ds) = (t, Two t1 t2 ds)
uncons (One t ds) =  (t, Two t1 t2 ds')
  where ((t1, t2), ds') = uncons ds

instance RandomAccessList BinaryList where

  empty = Nil

  isEmpty Nil = True
  isEmpty _ = False

  cons x Nil = One x Nil
  cons x (One y ps) = Two x y ps
  cons x (Two y z ps) = Three x y z ps
  cons x (Three y z w ps) = Two x y (cons (z, w) ps)

  head xs = fst (uncons xs)

  tail xs = snd (uncons xs)

  lookup i xs = undefined

  update i y xs = undefined
