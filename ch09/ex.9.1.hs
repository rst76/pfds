import Prelude hiding (drop)
import Data.List

data Tree a = Leaf a | Node Int (Tree a) (Tree a) deriving Show
data Digit a = Zero | One (Tree a) deriving Show
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
consTree t (Zero : ts) = One t : ts
consTree t1 (One t2 : ts) = Zero : consTree (link t1 t2) ts

cons :: a -> BinaryList a -> BinaryList a
cons x ts = consTree (Leaf x) ts

dropTree :: Int -> Tree a -> BinaryList a
dropTree 0 t = [One t]
dropTree 1 (Leaf _) = []
dropTree i t@(Node w t1 t2)
  | i <= w `div` 2 = One t2 : dropTree i t1
  | otherwise = Zero : dropTree (i - w `div` 2) t2

drop :: Int -> BinaryList a -> BinaryList a
drop i ts = reverse $ dropWhile isZero $ drop' i ts []
  where
  drop' 0 ts rs = reverse ts ++ rs
  drop' _ [] rs = rs
  drop' i (Zero : ts) rs = drop' i ts (Zero : rs)
  drop' i (One t : ts) rs
    | i >= size t = drop' (i - size t) ts (Zero : rs)
    | otherwise = reverse ts ++ Zero : dropTree i t
  isZero Zero = True
  isZero _ = False

create :: Int -> a -> BinaryList a
create n x = unfoldr inc (n `divMod` 2, Leaf x)
  where
  inc ((0, 0), _) = Nothing
  inc ((q, r), t) = Just (
    if r > 0 then One t else Zero,
    (q `divMod` 2, Node (size t * 2) t t))
