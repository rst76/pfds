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
-- Changed only the following line from ex.3.9.
link r (Two a1 t1 a2 t2) = T B (T R t2 a2 t1) a1 r

fromOrdList :: [a] -> Tree a
fromOrdList = foldl link E . foldl add []

main :: IO ()
main = mapM_ print [fromOrdList [1..n] | n <- [1..15]]
-- => T B E 1 E
-- => T B (T R E 1 E) 2 E
-- => T B (T B E 1 E) 2 (T B E 3 E)
-- => T B (T B E 1 E) 2 (T B (T R E 3 E) 4 E)
-- => T B (T R (T B E 1 E) 2 (T B E 3 E)) 4 (T B E 5 E)
-- => T B (T R (T B E 1 E) 2 (T B E 3 E)) 4 (T B (T R E 5 E) 6 E)
-- => T B (T B (T B E 3 E) 2 (T B E 1 E)) 4 (T B (T B E 5 E) 6 (T B E 7 E))
-- => T B (T B (T B E 3 E) 2 (T B E 1 E)) 4 (T B (T B E 5 E) 6 (T B (T R E 7 E) 8 E))
-- => T B (T B (T B E 3 E) 2 (T B E 1 E)) 4 (T B (T R (T B E 5 E) 6 (T B E 7 E)) 8 (T B E 9 E))
-- => T B (T B (T B E 3 E) 2 (T B E 1 E)) 4 (T B (T R (T B E 5 E) 6 (T B E 7 E)) 8 (T B (T R E 9 E) 10 E))
-- => T B (T R (T B (T B E 3 E) 2 (T B E 1 E)) 4 (T B (T B E 7 E) 6 (T B E 5 E))) 8 (T B (T B E 9 E) 10 (T B E 11 E))
-- => T B (T R (T B (T B E 3 E) 2 (T B E 1 E)) 4 (T B (T B E 7 E) 6 (T B E 5 E))) 8 (T B (T B E 9 E) 10 (T B (T R E 11 E) 12 E))
-- => T B (T R (T B (T B E 3 E) 2 (T B E 1 E)) 4 (T B (T B E 7 E) 6 (T B E 5 E))) 8 (T B (T R (T B E 9 E) 10 (T B E 11 E)) 12 (T B E 13 E))
-- => T B (T R (T B (T B E 3 E) 2 (T B E 1 E)) 4 (T B (T B E 7 E) 6 (T B E 5 E))) 8 (T B (T R (T B E 9 E) 10 (T B E 11 E)) 12 (T B (T R E 13 E) 14 E))
-- => T B (T B (T B (T B E 7 E) 6 (T B E 5 E)) 4 (T B (T B E 3 E) 2 (T B E 1 E))) 8 (T B (T B (T B E 11 E) 10 (T B E 9 E)) 12 (T B (T B E 13 E) 14 (T B E 15 E)))
