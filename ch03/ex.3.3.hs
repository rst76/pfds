data Heap a = E | T Int a (Heap a) (Heap a) deriving Show

rank :: Heap a -> Int
rank E = 0
rank (T r _ _ _ ) = r

makeT :: a -> Heap a -> Heap a -> Heap a
makeT x a b
  | rank a >= rank b = T (rank b + 1) x a b
  | otherwise        = T (rank a + 1) x b a

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h E = h
merge E h = h
merge h1@(T _ x a1 b1) h2@(T _ y a2 b2)
  | x <= y    = makeT x a1 (merge b1 h2)
  | otherwise = makeT y a2 (merge h1 b2)

fromList :: Ord a => [a] -> Heap a
fromList s = fromList' (map (\x -> T 1 x E E) s)
  where
  fromList' [t] = t
  fromList' s = fromList' (pair s)
  pair (a : b : s) = merge a b : pair s
  pair s = s

main :: IO ()
main = print $ fromList [1, 2, 3, 4, 5, 6, 7]
-- => T 2 1 (T 2 3 (T 2 5 (T 1 6 E E) (T 1 7 E E)) (T 1 4 E E)) (T 1 2 E E)
