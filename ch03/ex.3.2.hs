data Heap a = E | T Int a (Heap a) (Heap a) deriving Show

rank :: Heap a -> Int
rank E = 0
rank (T r _ _ _ ) = r

makeT :: a -> Heap a -> Heap a -> Heap a
makeT x a b
  | rank a >= rank b = T (rank b + 1) x a b
  | otherwise        = T (rank a + 1) x b a

insert :: Ord a => a -> Heap a -> Heap a
insert x E = T 1 x E E
insert x t@(T r y a b)
  | x <= y    = T 1 x t E
  | otherwise = makeT y a (insert x b)
