data Heap a = E | T Int a (Heap a) (Heap a) deriving Show

empty :: Heap a
empty = E

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

main :: IO ()
main = do
  print $ foldr insert empty [1, 2, 3, 4, 5, 6, 7]
  -- => T 1 1 (T 1 2 (T 1 3 (T 1 4 (T 1 5 (T 1 6 (T 1 7 E E) E) E) E) E) E) E
  print $ foldr insert empty "ebdac"
  -- => T 2 'a' (T 2 'b' (T 1 'd' E E) (T 1 'e' E E)) (T 1 'c' E E)
