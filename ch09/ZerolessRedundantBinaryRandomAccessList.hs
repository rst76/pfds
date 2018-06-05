module ZerolessRedundantBinaryRandomAccessList where

import Prelude hiding (head, tail, lookup)
import RandomAccessList

data Tree a = Leaf a | Node (Tree a) (Tree a)
data Digit a = One (Tree a) | Two (Tree a) (Tree a) | Three (Tree a) (Tree a) (Tree a)
newtype RList a = RL [Digit a]

consTree :: Tree a -> [Digit a] -> [Digit a]
consTree t [] = [One t]
consTree t (One t1 : ds) = Two t t1 : ds
consTree t (Two t1 t2 : ds) = Three t t1 t2 : ds
consTree t (Three t1 t2 t3 : ds) = Two t t1 : consTree (Node t2 t3) ds

unconsTree :: [Digit a] -> (Tree a, [Digit a])
unconsTree [] = error "empty list"
unconsTree [One t] = (t, [])
unconsTree (Two t t1 : ds) = (t, One t1 : ds)
unconsTree (Three t t1 t2 : ds) = (t, Two t1 t2 : ds)
unconsTree (One t : ds) =  (t, Two t1 t2 : ds')
  where (Node t1 t2, ds') = unconsTree ds

instance RandomAccessList RList where

  empty = RL []

  isEmpty (RL ds) = null ds
  
  cons x (RL ds) = RL (consTree (Leaf x) ds)

  head (RL ds) = x
    where (Leaf x, _) = unconsTree ds

  tail (RL ds) = RL ds'
    where (_, ds') = unconsTree ds

  lookup i (RL ds) = undefined

  update i y (RL ds) = undefined
