import Prelude hiding (head, tail, lookup)

data Tree a = Node a [Tree a] deriving Show
data Digit a = Zero | Ones [Tree a] | Two (Tree a) (Tree a) deriving Show
type Heap a = [Digit a]

empty :: Heap a
empty = []

link :: Ord a => Tree a -> Tree a -> Tree a
link t1@(Node x1 c1) t2@(Node x2 c2)
  | x1 <= x2  = Node x1 (t2 : c1)
  | otherwise = Node x2 (t1 : c2)

ones :: [Tree a] -> Heap a -> Heap a
ones [] ds = ds
ones ts (Ones ts1 : ds) = Ones (ts ++ ts1) : ds
ones ts ds = Ones ts : ds

fixup :: Ord a => Heap a -> Heap a
fixup (Ones ts : ds) = Ones ts : fixup ds
fixup (Two t1 t2 : ds) = Zero : simpleInc (link t1 t2) ds
fixup ds = ds

simpleInc :: Tree a -> Heap a -> Heap a
simpleInc t [] = [Ones [t]]
simpleInc t (Zero : ds) = ones [t]  ds
simpleInc t (Ones (t1 : ts) : ds) = Two t t1 : ones ts ds

insTree :: Ord a => Tree a -> Heap a -> Heap a
insTree t ds = fixup (simpleInc t ds)

insert :: Ord a => a -> Heap a -> Heap a
insert x = insTree (Node x [])

merge :: Ord a => Heap a -> Heap a -> Heap a
merge ds1 [] = ds1
merge [] ds2 = ds2
merge (Zero : ds1) (Zero : ds2) = Zero : merge ds1 ds2
merge (Zero : ds1) (Ones (t : ts) : ds2) = ones [t] (merge ds1 (ones ts ds2))
merge (Ones (t : ts) : ds1) (Zero : ds2) = ones [t] (merge (ones ts ds1) ds2)
merge (Ones (t1 : ts1) : ds1) (Ones (t2 : ts2) : ds2) =
  Zero : insTree (link t1 t2) (merge (ones ts1 ds1) (ones ts2 ds2))
merge (Zero : ds1) (Two t1 t2 : ds2) = Two t1 t2 : merge ds1 ds2
merge (Two t1 t2 : ds1) (Zero : ds2) = Two t1 t2 : merge ds1 ds2
merge (Ones (t : ts) : ds1) (Two t1 t2 : ds2) =
  ones [t] (insTree (link t1 t2) (merge (ones ts ds1) ds2))
merge (Two t1 t2 : ds1) (Ones (t : ts) : ds2) =
  ones [t] (insTree (link t1 t2) (merge ds1 (ones ts ds2)))
merge (Two t1 t2 : ds1) (Two t3 t4 : ds2) =
  Two t1 t2 : insTree (link t3 t4) (merge ds1 ds2)

removeMinTree :: Ord a => Heap a -> (Tree a, Heap a)
removeMinTree [] = error "empty heap"
removeMinTree [Ones [t]] = (t, [])
removeMinTree [Two t1@(Node x1 _) t2@(Node x2 _)]
  | x1 <= x2 = (t1, [Ones [t2]])
  | otherwise = (t2, [Ones [t1]])
removeMinTree (Zero : ds) = (t', Zero : ds')
  where (t', ds') = removeMinTree ds
removeMinTree (Ones (t@(Node x _) : ts) : ds)
  | x <= x' = (t, Zero : ones ts ds)
  | otherwise = (t', ones [t] ds')
  where (t'@(Node x' _), ds') = removeMinTree (ones ts ds)
removeMinTree (Two t1@(Node x1 _) t2@(Node x2 _) : ds)
  | x1 <= x2 && x1 <= x' = (t1, ones [t2] ds)
  | x2 <= x1 && x2 <= x' = (t2, ones [t1] ds)
  | otherwise = (t', Two t1 t2 : ds')
  where (t'@(Node x' _), ds') = removeMinTree ds

findMin :: Ord a => Heap a -> a
findMin ds = x
  where (Node x _, _) = removeMinTree ds

deleteMin :: Ord a => Heap a -> Heap a
deleteMin ds = merge (ones (reverse ts') []) ds'
  where (Node _ ts', ds') = removeMinTree ds
