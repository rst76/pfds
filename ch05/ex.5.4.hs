data Tree a = E | T (Tree a) a (Tree a) deriving Show

bigger :: Ord a => a -> Tree a -> Tree a
bigger _ E = E
bigger pivot (T a x b)
  | x <= pivot = bigger pivot b
bigger pivot (T E x b) = T E x b
bigger pivot (T (T a1 y a2) x b)
  | y <= pivot = T (bigger pivot a2) x b
  | otherwise  = T (bigger pivot a1) y (T a2 x b)

smaller :: Ord a => a -> Tree a -> Tree a
smaller _ E = E
smaller pivot (T a x b)
  | x > pivot = smaller pivot a
smaller pivot (T a x E) = T a x E
smaller pivot (T a x (T b1 y b2))
  | y > pivot = T a x (smaller pivot b1)
  | otherwise = T (T a x b1) y (smaller pivot b2)

insert :: Ord a => a -> Tree a -> Tree a
insert x t = T (smaller x t) x (bigger x t)

main :: IO ()
main = do
  print $ insert 0 (T (T (T (T (T (T (T E 1 E) 2 E) 3 E) 4 E) 5 E) 6 E) 7 E)
  -- => T E 0 (T (T (T (T E 1 E) 2 (T E 3 E)) 4 (T E 5 E)) 6 (T E 7 E))
  print $ insert 8 (T (T (T (T (T (T (T E 1 E) 2 E) 3 E) 4 E) 5 E) 6 E) 7 E)
  -- => T (T (T (T (T (T (T (T E 1 E) 2 E) 3 E) 4 E) 5 E) 6 E) 7 E) 8 E
  print $ insert 8 (T E 1 (T E 2 (T E 3 (T E 4 (T E 5 (T E 6 (T E 7 E)))))))
  -- => T (T (T E 1 E) 2 (T (T E 3 E) 4 (T (T E 5 E) 6 (T E 7 E)))) 8 E
  print $ insert 0 (T E 1 (T E 2 (T E 3 (T E 4 (T E 5 (T E 6 (T E 7 E)))))))
  -- => T E 0 (T E 1 (T E 2 (T E 3 (T E 4 (T E 5 (T E 6 (T E 7 E)))))))
