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

main :: IO ()
main = do
  let ts = foldr insert empty [1, 2, 3, 4, 5, 6, 7]
  print ts
  -- => [(0,Node 1 []),(1,Node 2 [Node 3 []]),(2,Node 4 [Node 6 [Node 7 []],Node 5 []])]
  print $ findMin ts
  -- => 1
  let ts = foldr insert empty "ebdac"
  print ts
  -- => [(0,Node 'e' []),(2,Node 'a' [Node 'b' [Node 'd' []],Node 'c' []])]
  print $ findMin ts
  -- => 'a'
