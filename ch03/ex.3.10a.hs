data Color = R | B deriving Show
data Tree a = E | T Color (Tree a) a (Tree a) deriving Show

empty :: Tree a
empty = E

member :: Ord a => a -> Tree a -> Bool
member x E = False
member x (T _ a y b)
  | x < y     = member x a
  | y < x     = member x b
  | otherwise = True

lbalance :: Color -> Tree a -> a -> Tree a -> Tree a
lbalance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
lbalance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
lbalance color a x b = T color a x b

rbalance :: Color -> Tree a -> a -> Tree a -> Tree a
rbalance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
rbalance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
rbalance color a x b = T color a x b

insert :: Ord a => a -> Tree a -> Tree a
insert x s = T B a y b
  where
  ins E = T R E x E
  ins s@(T color a y b)
    | x < y = lbalance color (ins a) y b
    | y < x = rbalance color a y (ins b)
    | otherwise = s
  T _ a y b = ins s

main = do
  print $ foldr insert empty "ebdac"
  -- => T B (T B E 'a' E) 'b' (T R (T B E 'c' E) 'd' (T B E 'e' E))
  print $ foldr insert empty [1 .. 7]
  -- => T B (T B (T B E 1 E) 2 (T B E 3 E)) 4 (T B (T B E 5 E) 6 (T B E 7 E))
  print $ foldr insert empty [7, 6 .. 1]
  -- => T B (T B (T B E 1 E) 2 (T B E 3 E)) 4 (T B (T B E 5 E) 6 (T B E 7 E))