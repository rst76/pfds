data BinTree a = E | T a (BinTree a) (BinTree a) deriving Show

empty :: BinTree a
empty = E

isEmpty :: BinTree a -> Bool
isEmpty E = True
isEmpty _ = False

merge :: Ord a => BinTree a -> BinTree a -> BinTree a
merge t E = t
merge E t = t
merge (T x t1 E) (T y t2 E) =
  if x <= y then T x (T y t2 t1) E else T y (T x t1 t2) E

insert :: Ord a => a -> BinTree a -> BinTree a
insert x = merge (T x E E)

mergePairs :: Ord a => BinTree a -> BinTree a
mergePairs (T x t1 (T y t2 t3)) = merge (merge (T x t1 E) (T y t2 E)) (mergePairs t3)
mergePairs t = t

findMin :: BinTree a -> a
findMin E = error "empty heap"
findMin (T x _ _) = x

deleteMin :: Ord a => BinTree a -> BinTree a
deleteMin E = error "empty heap"
deleteMin (T _ t E) = mergePairs t

main :: IO ()
main = do
  let h1 = foldl (flip insert) empty [5,4,3,2,1] 
  let h2 = foldl (flip insert) empty [1,2,3,4,5]
  let hs3 = scanl (flip insert) empty [2,4,1,3,5]
  print h1
  -- => T 1 (T 2 (T 3 (T 4 (T 5 E E) E) E) E) E
  print h2
  -- => T 1 (T 5 E (T 4 E (T 3 E (T 2 E E)))) E
  mapM_ print hs3
  -- => E
  -- => T 2 E E
  -- => T 2 (T 4 E E) E
  -- => T 1 (T 2 (T 4 E E) E) E
  -- => T 1 (T 3 E (T 2 (T 4 E E) E)) E
  -- => T 1 (T 5 E (T 3 E (T 2 (T 4 E E) E))) E
  print $ deleteMin h1
  -- => T 2 (T 3 (T 4 (T 5 E E) E) E) E
  print $ deleteMin h2
  -- => T 2 (T 4 (T 5 E E) (T 3 E E)) E
  mapM_ print $ take 6 $ iterate deleteMin $ last hs3
  -- => T 1 (T 5 E (T 3 E (T 2 (T 4 E E) E))) E
  -- => T 2 (T 3 (T 5 E E) (T 4 E E)) E
  -- => T 3 (T 4 E (T 5 E E)) E
  -- => T 4 (T 5 E E) E
  -- => T 5 E E
  -- => E
