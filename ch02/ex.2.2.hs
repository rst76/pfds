data Tree a = E | T (Tree a) a (Tree a)

member :: Ord a => a -> Tree a -> Bool
member _ E = False
member x t@(T _ y _) = member' y t
  where
  member' z E = x == z
  member' z (T left y right)
    | x < y     = member' z left
    | otherwise = member' y right