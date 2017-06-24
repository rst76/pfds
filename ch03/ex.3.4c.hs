data Heap a = E | T Int a (Heap a) (Heap a) deriving Show

rank :: Heap a -> Int
rank E = 0
rank (T r _ _ _ ) = r

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h E = h
merge E h = h
merge h1@(T r1 x a1 b1) h2@(T r2 y a2 b2)
  | x <= y, rank a1 >= rank b1 + rank h2 = T (r1 + r2) x a1 (merge b1 h2)
  | x <= y                               = T (r1 + r2) x (merge b1 h2) a1
  | otherwise                            = merge h2 h1

insert :: Ord a => a -> Heap a -> Heap a
insert x h = merge (T 1 x E E) h
