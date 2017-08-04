import Data.List(inits)

data Heap a = E | T (Heap a) a (Heap a) deriving Show

empty :: Heap a
empty = E

isEmpty :: Heap a -> Bool
isEmpty E = True
isEmpty _ = False

partition :: Ord a => a -> Heap a -> (Heap a, Heap a)
partition _ E = (E, E)
partition pivot t@(T a x b)
  | x <= pivot = case b of
    E -> (t, E)
    T b1 y b2 ->
      if y <= pivot then
        let (small, big) = partition pivot b2
        in (T (T a x b1) y small, big)
      else
        let (small, big) = partition pivot b1
        in (T a x small, T big y b2)
  | otherwise = case a of
    E -> (E, t)
    T a1 y a2 ->
      if y <= pivot then
        let (small, big) = partition pivot a2
        in (T a1 y small, T big x b)
      else
        let (small, big) = partition pivot a1
        in (small, T big y (T a2 x b))

insert :: Ord a => a -> Heap a -> Heap a
insert x t = let (a, b) = partition x t in T a x b

fromList :: Ord a => [a] -> Heap a
fromList = foldl (flip insert) empty

toList :: Ord a => Heap a -> [a]
toList E = []
toList (T a x b) = toList a ++ [x] ++ toList b

splaySort :: Ord a => [a] -> [a]
splaySort = toList . fromList

main :: IO ()
main = do
  mapM_ (print . fromList) (inits [1,2,3,4,5])
  -- => E
  -- => T E 1 E
  -- => T (T E 1 E) 2 E
  -- => T (T (T E 1 E) 2 E) 3 E
  -- => T (T (T (T E 1 E) 2 E) 3 E) 4 E
  -- => T (T (T (T (T E 1 E) 2 E) 3 E) 4 E) 5 E
  mapM_ (print . fromList) (inits [5,4,3,2,1])
  -- => E
  -- => T E 5 E
  -- => T E 4 (T E 5 E)
  -- => T E 3 (T E 4 (T E 5 E))
  -- => T E 2 (T E 3 (T E 4 (T E 5 E)))
  -- => T E 1 (T E 2 (T E 3 (T E 4 (T E 5 E))))
  print $ splaySort [1,2,3,4,5]
  -- => [1,2,3,4,5]
  print $ splaySort [5,4,3,2,1]
  -- => [1,2,3,4,5]
  print $ splaySort [2,5,3,1,4]
  -- => [1,2,3,4,5]
