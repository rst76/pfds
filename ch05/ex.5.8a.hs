data Heap a = E | T a [Heap a] deriving Show
data BinTree a = E' | T' a (BinTree a) (BinTree a) deriving Show

empty :: Heap a
empty = E

isEmpty :: Heap a -> Bool
isEmpty E = True
isEmpty _ = False

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h E = h
merge E h = h
merge h1@(T x hs1) h2@(T y hs2) =
  if x <= y then T x (h2 : hs1) else T y (h1 : hs2)

insert :: Ord a => a -> Heap a -> Heap a
insert x = merge (T x [])

mergePairs :: Ord a => [Heap a] -> Heap a
mergePairs [] = E
mergePairs [h] = h
mergePairs (h1 : h2 : hs) = merge (merge h1 h2) (mergePairs hs)

findMin :: Heap a -> a
findMin E = error "empty heap"
findMin (T x _) = x

deleteMin :: Ord a => Heap a -> Heap a
deleteMin E = error "empty heap"
deleteMin (T _ hs) = mergePairs hs

toBinary :: Heap a -> BinTree a
toBinary E = E'
toBinary h = addLeft h E'
  where
  addLeft (T x hs) = T' x (foldr addLeft E' hs)

main :: IO ()
main = do
  let h1 = foldl (flip insert) empty [5,4,3,2,1] 
  let h2 = foldl (flip insert) empty [1,2,3,4,5]
  let hs3 = scanl (flip insert) empty [2,4,1,3,5]
  print h1
  -- => T 1 [T 2 [T 3 [T 4 [T 5 []]]]]
  print h2
  -- => T 1 [T 5 [],T 4 [],T 3 [],T 2 []]
  mapM_ print hs3
  -- => E
  -- => T 2 []
  -- => T 2 [T 4 []]
  -- => T 1 [T 2 [T 4 []]]
  -- => T 1 [T 3 [],T 2 [T 4 []]]
  -- => T 1 [T 5 [],T 3 [],T 2 [T 4 []]]
  print $ deleteMin h1
  -- => T 2 [T 3 [T 4 [T 5 []]]]
  print $ deleteMin h2
  -- => T 2 [T 4 [T 5 []],T 3 []]
  mapM_ print $ take 6 $ iterate deleteMin $ last hs3
  -- => T 1 [T 5 [],T 3 [],T 2 [T 4 []]]
  -- => T 2 [T 3 [T 5 []],T 4 []]
  -- => T 3 [T 4 [],T 5 []]
  -- => T 4 [T 5 []]
  -- => T 5 []
  -- => E
  print $ toBinary $ last hs3
  -- => T' 1 (T' 5 E' (T' 3 E' (T' 2 (T' 4 E' E') E'))) E'
