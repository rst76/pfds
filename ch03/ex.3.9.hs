data Color = R | B deriving Show
data Tree a = E | T Color (Tree a) a (Tree a) deriving Show

data Digit a = One a (Tree a) | Two a (Tree a) a (Tree a)

incr :: Digit a -> [Digit a] -> [Digit a]
incr (One a t) [] = [One a t]
incr (One a1 t1) (One a2 t2 : ps) = Two a1 t1 a2 t2 : ps
incr (One a1 t1) (Two a2 t2 a3 t3 : ps) = One a1 t1 : incr (One a2 (T B t2 a3 t3)) ps

add :: [Digit a] -> a -> [Digit a]
add ps a = incr (One a E) ps

link :: Tree a -> Digit a -> Tree a
link r (One a t) = T B t a r
link r (Two a1 t1 a2 t2) = T B t2 a2 (T R t1 a1 r)

-- Based on the paper "Constructing Red-Black Trees",
-- but constructs tree from left (foldl).
fromOrdList :: [a] -> Tree a
fromOrdList = foldl link E . foldl add []

main :: IO ()
main = mapM_ print [fromOrdList [1..n] | n <- [1..8]]
-- => T B E 1 E
-- => T B E 1 (T R E 2 E)
-- => T B (T B E 1 E) 2 (T B E 3 E)
-- => T B (T B E 1 E) 2 (T B E 3 (T R E 4 E))
-- => T B (T B E 1 E) 2 (T R (T B E 3 E) 4 (T B E 5 E))
-- => T B (T B E 1 E) 2 (T R (T B E 3 E) 4 (T B E 5 (T R E 6 E)))
-- => T B (T B (T B E 3 E) 2 (T B E 1 E)) 4 (T B (T B E 5 E) 6 (T B E 7 E))
-- => T B (T B (T B E 3 E) 2 (T B E 1 E)) 4 (T B (T B E 5 E) 6 (T B E 7 (T R E 8 E)))

-- Consider the number of calling "incr".
-- It occurs at least once for one insertion.
-- And it occurs once for one carring in radix-2 notation.
-- (If tree's size increase {2} to {11} (2 to 3), there happens one more "incr" calling.)
-- As the carring at the i-th digit occurs at most once per 2^i times,
-- total calling number is n + n/2 + n/4 + .... <= 2n
-- So, fromOrdList run in O(n) time.
