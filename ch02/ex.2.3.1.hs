data Tree a = E | T (Tree a) a (Tree a) deriving Show

insert :: Ord a => a -> Tree a -> Tree a
insert x t = insert' id t
  where
  insert' k E = k (T E x E)
  insert' k (T left y right)
    | x < y = insert' (\l -> k (T l y right)) left
    | x > y = insert' (\r -> k (T left y r)) right
    | otherwise = t
