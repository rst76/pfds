data Heap a = E | T Int a (Heap a) (Heap a) deriving Show

rank :: Heap a -> Int
rank E = 0
rank (T r _ _ _ ) = r

makeT :: a -> Heap a -> Heap a -> Heap a
makeT x a b
  | rank a >= rank b = T (rank a + rank b + 1) x a b
  | otherwise        = T (rank a + rank b + 1) x b a

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h E = h
merge E h = h
merge h1@(T _ x a1 b1) h2@(T _ y a2 b2)
  | x <= y    = makeT x a1 (merge b1 h2)
  | otherwise = makeT y a2 (merge h1 b2)

insert :: Ord a => a -> Heap a -> Heap a
insert x h = merge (T 1 x E E) h
