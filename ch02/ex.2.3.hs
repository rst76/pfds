data Tree a = E | T (Tree a) a (Tree a) deriving Show

insert :: Ord a => a -> Tree a -> Tree a
insert x t = case insert' t of
  Nothing -> t
  Just t' -> t'
  where
  insert' E = Just (T E x E)
  insert' (T left y right)
    | x < y = fmap (\l -> T l y right) (insert' left)
    | x > y = fmap (\r -> T left y r)  (insert' right)
    | otherwise = Nothing