import Prelude hiding (head, tail, lookup)

data Tree a = Leaf a | Node Int (Tree a) (Tree a) deriving Show
type BinaryList a = [Tree a]

empty :: BinaryList a
empty = []

size :: Tree a -> Int
size (Leaf _) = 1
size (Node w t1 t2) = w

link :: Tree a -> Tree a -> Tree a
link t1 t2 = Node (size t1 + size t2) t1 t2

consTree :: Tree a -> BinaryList a -> BinaryList a
consTree t [] = [t]
consTree t1 (t2 : ts)
  | size t1 < size t2 = t1 : t2 : ts
  | otherwise = consTree (link t1 t2) ts

cons :: a -> BinaryList a -> BinaryList a
cons x ts = consTree (Leaf x) ts

unconsTree :: BinaryList a -> (Tree a, BinaryList a)
unconsTree [] = error "empty list"
unconsTree (Leaf x : ts) = (Leaf x, ts)
unconsTree (Node _ t1 t2 : ts) = unconsTree (t1 : t2 : ts)

head :: BinaryList a -> a
head ts = let (Leaf x, _) = unconsTree ts in x

tail :: BinaryList a -> BinaryList a
tail ts = let (_, ts') = unconsTree ts in ts'

lookupTree :: Int -> Tree a -> a
lookupTree 0 (Leaf x) = x
lookupTree _ (Leaf _) = error "bad subscript"
lookupTree i (Node w t1 t2)
  | i < w `div` 2 = lookupTree i t1
  | otherwise = lookupTree (i - w `div` 2) t2

lookup :: Int -> BinaryList a -> a
lookup _ [] = error "bad subscript"
lookup i (t : ts)
  | i < size t = lookupTree i t
  | otherwise = lookup (i - size t) ts

updateTree :: Int -> a -> Tree a -> Tree a
updateTree 0 y (Leaf _) = Leaf y
updateTree i _ (Leaf _) = error "bad subscript"
updateTree i y (Node w t1 t2)
  | i < w `div` 2 = Node w (updateTree i y t1) t2
  | otherwise = Node w t1 (updateTree (i - w `div` 2) y t2)

update :: Int -> a -> BinaryList a -> BinaryList a
update _ _ [] = error "bad subscript"
update i y (t : ts)
  | i < size t = updateTree i y t : ts
  | otherwise = t : update (i - size t) y ts
