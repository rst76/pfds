import Prelude hiding (head, tail, lookup)

data Tree a = Leaf a | Node Int (Tree a) (Tree a) deriving Show
data Digit a = One (Tree a) | Two (Tree a) (Tree a) deriving Show
type RList a = [Digit a]

empty :: RList a
empty = []

size :: Tree a -> Int
size (Leaf _) = 1
size (Node w _ _) = w

link :: Tree a -> Tree a -> Tree a
link t1 t2 = Node (size t1 + size t2) t1 t2

consTree :: Tree a -> RList a -> RList a
consTree t [] = [One t]
consTree t (One t1 : ds) = Two t t1 : ds
consTree t (Two t1 t2 : ds) = One t : consTree (link t1 t2) ds

cons :: a -> RList a -> RList a
cons x ds = consTree (Leaf x) ds

unconsTree :: RList a -> (Tree a, RList a)
unconsTree [] = error "empty list"
unconsTree [One t] = (t, [])
unconsTree (Two t1 t2 : ds) = (t1, One t2 : ds)
unconsTree (One t : ds) =  (t, Two t1 t2 : ds')
  where (Node _ t1 t2, ds') = unconsTree ds

head :: RList a -> a
head ds = x
  where (Leaf x, _) = unconsTree ds

tail :: RList a -> RList a
tail ds = ds'
  where (_, ds') = unconsTree ds

lookupTree :: Int -> Tree a -> a
lookupTree 0 (Leaf x) = x
lookupTree _ (Leaf _) = error "bad subscript"
lookupTree i (Node w t1 t2)
  | i < w `div` 2 = lookupTree i t1
  | otherwise = lookupTree (i - w `div` 2) t2

lookup :: Int -> RList a -> a
lookup _ [] = error "bad subscript"
lookup i (One t : ds)
  | i < size t = lookupTree i t
  | otherwise = lookup (i - size t) ds
lookup i (Two t1 t2 : ds)
  | i < size t1 = lookupTree i t1
  | i < size t1 + size t2 = lookupTree i t2
  | otherwise = lookup (i - size t1 - size t2) ds

updateTree :: Int -> a -> Tree a -> Tree a
updateTree 0 y (Leaf _) = Leaf y
updateTree i _ (Leaf _) = error "bad subscript"
updateTree i y (Node w t1 t2)
  | i < w `div` 2 = Node w (updateTree i y t1) t2
  | otherwise = Node w t1 (updateTree (i - w `div` 2) y t2)

update :: Int -> a -> RList a -> RList a
update _ _ [] = error "bad subscript"
update i y (One t : ds)
  | i < size t = One (updateTree i y t) : ds
  | otherwise = One t : update (i - size t) y ds
update i y (Two t1 t2 : ds)
  | i < size t1 = Two (updateTree i y t1) t2 : ds
  | i < size t1 + size t2 = Two t1 (updateTree i y t2) : ds
  | otherwise = Two t1 t2 : update (i - size t1 - size t2) y ds
