data Color = R | B deriving Show
data Tree a = E | T [Int] Color (Tree a) (Bool, a) (Tree a) deriving Show
data Digit a = One a (Tree a) | Two a (Tree a) a (Tree a)

empty :: Tree a
empty = E

member :: Ord a => a -> Tree a -> Bool
member x E = False
member x (T _ _ a (p, y) b)
  | x < y     = member x a
  | y < x     = member x b
  | otherwise = p

balance :: Tree a -> Tree a
-- Ignore the number of elements.
balance (T _ B (T _ R (T _ R a x b) y c) z d) = T [] R (T [] B a x b) y (T [] B c z d)
balance (T _ B (T _ R a x (T _ R b y c)) z d) = T [] R (T [] B a x b) y (T [] B c z d)
balance (T _ B a x (T _ R (T _ R b y c) z d)) = T [] R (T [] B a x b) y (T [] B c z d)
balance (T _ B a x (T _ R b y (T _ R c z d))) = T [] R (T [] B a x b) y (T [] B c z d)
balance t = t

insert :: Ord a => a -> Tree a -> Tree a
insert x E = T [0, 1] R E (True, x) E
insert x s@(T [i, j] _ _ _ _) = T [i, j + 1] B a y b
  where
  ins E = T [] R E (True, x) E
  ins s@(T _ color a (p, y) b)
    | x < y     = balance (T [] color (ins a) (p, y) b)
    | y < x     = balance (T [] color a (p, y) (ins b))
    | otherwise = T [] color a (True, y) b
  T _ _ a y b = ins s

size :: Tree a -> Int
size E = 0
size (T [_, j] _ _ _ _) = j

incr :: Digit a -> [Digit a] -> [Digit a]
incr (One a t) [] = [One a t]
incr (One a1 t1) (One a2 t2 : ps) = Two a1 t1 a2 t2 : ps
incr (One a1 t1) (Two a2 t2 a3 t3 : ps) =
  One a1 t1 : incr (One a2 (T [0, size t2 + size t3 + 1] B t2 (True, a3) t3)) ps

add :: [Digit a] -> a -> [Digit a]
add ps a = incr (One a E) ps

link :: Tree a -> Digit a -> Tree a
link r (One a t) = T [0, size r + size t + 1] B t (True, a) r
link r (Two a1 t1 a2 t2) =
  T [0, size r + size t1 + size t2 + 2] B t2 (True, a2) (T [0, size r + size t1 + 1] R t1 (True, a1) r)

-- Based on the paper "Constructing Red-Black Trees",
-- but constructs tree from left (foldl).
fromOrdList :: [a] -> Tree a
fromOrdList = foldl link E . foldl add []

toOrdList :: Tree a -> [a]
toOrdList E = []
toOrdList (T _ _ a (p, x) b) = toOrdList a ++ [x | p] ++ toOrdList b

rebuild :: Ord a => Tree a -> Tree a
rebuild = fromOrdList . toOrdList

delete :: Ord a => a -> Tree a -> Tree a
delete x E = E
delete x s@(T [i, j] _ _ _ _)
  | i + 1 > j `div` 2 = rebuild $ del s
  | otherwise = T [i + 1, j] B a y b
  where
  del E = E
  del s@(T _ color a (p, y) b)
    | x < y     = T [] color (del a) (p, y) b
    | y < x     = T [] color a (p, y) (del b)
    | otherwise = T [] color a (False, y) b
  T _ _ a y b = del s

main = do
  print $ flip (foldr delete) "ab" $ foldr insert empty "ebdac"
  -- => T [2,5] B (T [] B E (False,'a') E) (False,'b') (T [] R (T [] B E (True,'c') E) (True,'d') (T [] B E (True,'e') E))
  print $ flip (foldr delete) "abc" $ foldr insert empty "ebdac"
  -- => T [0,2] B E (True,'d') (T [0,1] R E (True,'e') E)
