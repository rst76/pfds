data Tree a = E | T (Tree a) a (Tree a) deriving Show

complete :: a -> Int -> Tree a
complete _ 0 = E
complete x d = T t x t
  where
  t = complete x (d - 1)
