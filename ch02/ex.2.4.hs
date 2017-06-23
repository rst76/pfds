data Tree a = E | T (Tree a) a (Tree a) deriving Show

insert :: Ord a => a -> Tree a -> Tree a
insert x E = T E x E
insert x t@(T _ z _) = insert' z id t
  where
  insert' z k E
    | x == z    = t
    | otherwise = k (T E x E)
  insert' z k (T left y right)
    | x < y     = insert' z (\l -> k (T l y right)) left
    | otherwise = insert' y (\r -> k (T left y r)) right
