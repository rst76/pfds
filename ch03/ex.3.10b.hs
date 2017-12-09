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

llbalance :: Color -> Tree a -> a -> Tree a -> Tree a
llbalance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
llbalance color a x b = T color a x b

lrbalance :: Color -> Tree a -> a -> Tree a -> Tree a
lrbalance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
lrbalance color a x b = T color a x b

rlbalance :: Color -> Tree a -> a -> Tree a -> Tree a
rlbalance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
rlbalance color a x b = T color a x b

rrbalance :: Color -> Tree a -> a -> Tree a -> Tree a
rrbalance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
rrbalance color a x b = T color a x b

insert :: Ord a => a -> Tree a -> Tree a
insert x s = T B a y b
  where
  ins E = ((T, T), T R E x E)
  ins s@(T color a y b)
    | x < y = let (f, a') = ins a in ((llbalance, rlbalance), fst f color a' y b)
    | y < x = let (f, b') = ins b in ((lrbalance, rrbalance), snd f color a y b')
    | otherwise = ((T, T), s)
  (_, T _ a y b) = ins s

main = do
  print $ foldr insert empty "ebdac"
  -- => T B (T B E 'a' E) 'b' (T R (T B E 'c' E) 'd' (T B E 'e' E))
  print $ foldr insert empty [1 .. 7]
  -- => T B (T B (T B E 1 E) 2 (T B E 3 E)) 4 (T B (T B E 5 E) 6 (T B E 7 E))
  print $ foldr insert empty [7, 6 .. 1]
  -- => T B (T B (T B E 1 E) 2 (T B E 3 E)) 4 (T B (T B E 5 E) 6 (T B E 7 E))
