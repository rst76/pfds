data Heap a = E | T Int a (Heap a) (Heap a) deriving Show

empty :: Heap a
empty = E

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

main :: IO ()
main = do
  print $ foldr insert empty [1, 2, 3, 4, 5, 6, 7]
  -- => T 7 1 (T 6 2 (T 5 3 (T 4 4 (T 3 5 (T 2 6 (T 1 7 E E) E) E) E) E) E) E
  print $ foldr insert empty "ebdac"
  -- => T 5 'a' (T 2 'b' (T 1 'd' E E) E) (T 2 'c' (T 1 'e' E E) E)
