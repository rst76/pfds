data Tree a = E | T (Tree a) a (Tree a) deriving Show

complete :: a -> Int -> Tree a
complete x m = fst $ complete2 m
  where
  complete2 0 = (E, T E x E)
  complete2 m
    | even m    = (T t0 x t1, T t1 x t1)
    | otherwise = (T t0 x t0, T t0 x t1)
    where
    (t0, t1) = complete2 ((m - 1) `div` 2)
