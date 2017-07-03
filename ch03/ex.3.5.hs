data Tree a = Node Int a [Tree a] deriving Show
type Heap a = [Tree a]

empty :: Heap a
empty = []

rank :: Tree a -> Int
rank (Node r _ _) = r

root :: Tree a -> a
root (Node _ x _) = x

link :: Ord a => Tree a -> Tree a -> Tree a
link t1@(Node r x1 c1) t2@(Node _ x2 c2)
  | x1 <= x2  = Node (r + 1) x1 (t2 : c1)
  | otherwise = Node (r + 1) x2 (t1 : c2)

insTree :: Ord a => Tree a -> Heap a -> Heap a
insTree t [] = [t]
insTree t ts@(t' : ts')
  | rank t < rank t' = t : ts
  | otherwise = insTree (link t t') ts'

insert :: Ord a => a -> Heap a -> Heap a
insert x = insTree (Node 0 x [])

findMin :: Ord a => Heap a -> a
-- findMin = minimum . map root
findMin [] = error "Emtpy!"
findMin [t] = root t
findMin (t : ts) = min (root t) (findMin ts)

main :: IO ()
main = do
  let ts = foldr insert empty [1,2,3,4,5,6,7]
  print ts
  -- => [Node 0 1 [],Node 1 2 [Node 0 3 []],Node 2 4 [Node 1 6 [Node 0 7 []],Node 0 5 []]]
  print $ findMin ts
  -- => 1
  let ts = foldr insert empty "ebdac"
  print ts
  -- => [Node 0 'e' [],Node 2 'a' [Node 1 'b' [Node 0 'd' []],Node 0 'c' []]]
  print $ findMin ts
  -- => 'a'
