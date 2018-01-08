import Prelude hiding (head, tail, lookup)

data Tree a = Leaf a | Node Int (Tree a) (Tree a) deriving Show
data Digit a = One (Tree a) | Two (Tree a) (Tree a) deriving Show
type BinaryList a = [Digit a]

empty :: BinaryList a
empty = []

size :: Tree a -> Int
size (Leaf _) = 1
size (Node w t1 t2) = w

link :: Tree a -> Tree a -> Tree a
link t1 t2 = Node (size t1 + size t2) t1 t2

consTree :: Tree a -> BinaryList a -> BinaryList a
consTree t [] = [One t]
consTree t1 (One t2 : ts) = Two t1 t2 : ts
consTree t (Two t1 t2 : ts) = One t : consTree (link t1 t2) ts

cons :: a -> BinaryList a -> BinaryList a
cons x ts = consTree (Leaf x) ts

head :: BinaryList a -> a
head [] = error "empty list"
head (One (Leaf x) : _) = x
head (Two (Leaf x) _ : _) = x

unconsTree :: BinaryList a -> (Tree a, BinaryList a)
unconsTree [] = error "empty list"
unconsTree [One t] = (t, [])
unconsTree (Two t1 t2 : ts) = (t1, One t2 : ts)
unconsTree (One t : ts) =  (t, Two t1 t2 : ts')
  where
  (Node _ t1 t2, ts') = unconsTree ts

tail :: BinaryList a -> BinaryList a
tail ts = snd $ unconsTree ts

lookupTree :: Int -> Tree a -> a
lookupTree 0 (Leaf x) = x
lookupTree _ (Leaf _) = error "bad subscript"
lookupTree i (Node w t1 t2)
  | i < w `div` 2 = lookupTree i t1
  | otherwise = lookupTree (i - w `div` 2) t2

lookup :: Int -> BinaryList a -> a
lookup _ [] = error "bad subscript"
lookup i (One t : ts)
  | i < size t = lookupTree i t
  | otherwise = lookup (i - size t) ts
lookup i (Two t1 t2 : ts)
  | i < size t1 = lookupTree i t1
  | i < size t1 + size t2 = lookupTree i t2
  | otherwise = lookup (i - size t1 - size t2) ts

updateTree :: Int -> a -> Tree a -> Tree a
updateTree 0 y (Leaf _) = Leaf y
updateTree i _ (Leaf _) = error "bad subscript"
updateTree i y (Node w t1 t2)
  | i < w `div` 2 = Node w (updateTree i y t1) t2
  | otherwise = Node w t1 (updateTree (i - w `div` 2) y t2)

update :: Int -> a -> BinaryList a -> BinaryList a
update _ _ [] = error "bad subscript"
update i y (One t : ts)
  | i < size t = One (updateTree i y t) : ts
  | otherwise = One t : update (i - size t) y ts
update i y (Two t1 t2 : ts)
  | i < size t1 = Two (updateTree i y t1) t2 : ts
  | i < size t1 + size t2 = Two t1 (updateTree i y t2) : ts
  | otherwise = Two t1 t2 : update (i - size t1 - size t2) y ts
