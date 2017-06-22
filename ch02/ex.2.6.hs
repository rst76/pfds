import Prelude hiding (lookup)

data Tree a = E | T (Tree a) a (Tree a) deriving Show
type Map k a = Tree (k, a)

empty :: Map k a
empty = E

bind :: Ord k => k -> a -> Map k a -> Map k a
bind kx x E = T E (kx, x) E
bind kx x (T left (ky, y) right)
  | kx < ky   = T (bind kx x left) (ky, y) right
  | kx > ky   = T left (ky, y) (bind kx x right)
  | otherwise = T left (kx, x) right

lookup :: Ord k => k -> Map k a -> a
lookup k E = error "Not Found!"
lookup k (T left (kx, x) right)
  | k < kx    = lookup k left
  | k > kx    = lookup k right
  | otherwise = x
