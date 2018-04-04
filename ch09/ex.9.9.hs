import Prelude hiding (head, tail, lookup)

data Tree a = Leaf a | Node (Tree a) (Tree a)
data Digit a = One (Tree a) | Two (Tree a) (Tree a) | Three (Tree a) (Tree a) (Tree a)
type RList a = [Digit a]

empty :: RList a
empty = []

consTree :: Tree a -> RList a -> RList a
consTree t [] = [One t]
consTree t (One t1 : ds) = Two t t1 : ds
consTree t (Two t1 t2 : ds) = Three t t1 t2 : ds
consTree t (Three t1 t2 t3 : ds) = Two t t1 : consTree (Node t2 t3) ds

cons :: a -> RList a -> RList a
cons x ds = consTree (Leaf x) ds

unconsTree :: RList a -> (Tree a, RList a)
unconsTree [] = error "empty list"
unconsTree [One t] = (t, [])
unconsTree (Two t t1 : ds) = (t, One t1 : ds)
unconsTree (Three t t1 t2 : ds) = (t, Two t1 t2 : ds)
unconsTree (One t : ds) =  (t, Two t1 t2 : ds')
  where (Node t1 t2, ds') = unconsTree ds

head :: RList a -> a
head ds = x
  where (Leaf x, _) = unconsTree ds

tail :: RList a -> RList a
tail ds = ds'
  where (_, ds') = unconsTree ds
