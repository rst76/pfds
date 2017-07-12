data Tree a = Node a [Tree a] deriving Show
type Heap a = [(Int, Tree a)]

empty :: Heap a
empty = []

root :: Tree a -> a
root (Node x _) = x

link :: Ord a => Tree a -> Tree a -> Tree a
link t1@(Node x1 c1) t2@(Node x2 c2)
  | x1 <= x2  = Node x1 (t2 : c1)
  | otherwise = Node x2 (t1 : c2)

insTree :: Ord a => (Int, Tree a) -> Heap a -> Heap a
insTree (r, t) [] = [(r, t)]
insTree (r, t) ts@((r', t') : ts')
  | r < r' = (r, t) : ts
  | otherwise = insTree (r + 1, link t t') ts'

insert :: Ord a => a -> Heap a -> Heap a
insert x = insTree (0, Node x [])

findMin :: Ord a => Heap a -> a
-- findMin = minimum . map (root . snd)
findMin [] = error "Emtpy!"
findMin [(_, t)] = root t
findMin ((_, t) : ts) = min (root t) (findMin ts)

merge :: Ord a => Heap a -> Heap a -> Heap a
merge ts1 [] = ts1
merge [] ts2 = ts2
merge ts1@((r1, t1) : ts1') ts2@((r2, t2) : ts2')
  | r1 < r2 = (r1, t1) : merge ts1' ts2
  | r2 < r1 = (r2, t2) : merge ts1 ts2'
  | otherwise = insTree (r1 + 1, link t1 t2) (merge ts1' ts2')

removeMinTree :: Ord a => Heap a -> (Tree a, Heap a)
removeMinTree [] = error "Emtpy!"
removeMinTree [(_, t)] = (t, [])
removeMinTree ((r, t) : ts)
  | root t <= root t' = (t, ts)
  | otherwise         = (t', (r, t) : ts')
  where
  (t', ts') = removeMinTree ts

deleteMin :: Ord a => Heap a -> Heap a
deleteMin ts = merge (zip [0..] (reverse ts1)) ts2
  where
  (Node x ts1, ts2) = removeMinTree ts

main :: IO ()
main = do
  let ts = foldr insert empty [1, 2, 3, 4, 5, 6, 7]
  print ts
  -- => [(0,Node 1 []),(1,Node 2 [Node 3 []]),(2,Node 4 [Node 6 [Node 7 []],Node 5 []])]
  print $ findMin ts
  -- => 1
  print $ deleteMin ts
  -- => [(1,Node 2 [Node 3 []]),(2,Node 4 [Node 6 [Node 7 []],Node 5 []])]
  let ts = foldr insert empty "ebdac"
  print ts
  -- => [(0,Node 'e' []),(2,Node 'a' [Node 'b' [Node 'd' []],Node 'c' []])]
  print $ findMin ts
  -- => 'a'
  print $ deleteMin ts
  -- => [(2,Node 'b' [Node 'c' [Node 'e' []],Node 'd' []])]
