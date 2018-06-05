import Prelude hiding (tail, lookup)
import RandomAccessList
import SkewTrinaryRandomAccessList

main :: IO ()
main = do
  let ns = [0 .. 12]
  let l = foldr cons empty ns :: SkewList Int
  print l
  -- 001
  -- SL [(13,Node 0 (Node 1 (Leaf 2) (Leaf 3) (Leaf 4)) (Node 5 (Leaf 6) (Leaf 7) (Leaf 8)) (Node 9 (Leaf 10) (Leaf 11) (Leaf 12)))]
  print $ tail l
  -- 03
  -- SL [(4,Node 1 (Leaf 2) (Leaf 3) (Leaf 4)),(4,Node 5 (Leaf 6) (Leaf 7) (Leaf 8)),(4,Node 9 (Leaf 10) (Leaf 11) (Leaf 12))]
  print $ map (`lookup` l) ns
  -- [0,1,2,3,4,5,6,7,8,9,10,11,12]
  print $ foldr (\i -> update i (i + 10)) l ns
  -- SL [(13,Node 10 (Node 11 (Leaf 12) (Leaf 13) (Leaf 14)) (Node 15 (Leaf 16) (Leaf 17) (Leaf 18)) (Node 19 (Leaf 20) (Leaf 21) (Leaf 22)))]
