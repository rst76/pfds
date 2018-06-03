import Prelude hiding (tail, lookup)
import RandomAccessList
import ZerolessQuaternaryRandomAccessList

main :: IO ()
main = do
  let ns = [0 .. 20]
  let l = foldr cons empty ns :: QuaternaryList Int
  print l
  -- 111
  -- QL [[Leaf 0],
  --     [Node [Leaf 1,Leaf 2,Leaf 3,Leaf 4]],
  --     [Node [Node [Leaf 5,Leaf 6,Leaf 7,Leaf 8],Node [Leaf 9,Leaf 10,Leaf 11,Leaf 12],Node [Leaf 13,Leaf 14,Leaf 15,Leaf 16],Node [Leaf 17,Leaf 18,Leaf 19,Leaf 20]]]]
  print $ tail l
  -- 44
  -- QL [[Node [Leaf 1,Leaf 2,Leaf 3,Leaf 4]],
  --     [Node [Node [Leaf 5,Leaf 6,Leaf 7,Leaf 8],Node [Leaf 9,Leaf 10,Leaf 11,Leaf 12],Node [Leaf 13,Leaf 14,Leaf 15,Leaf 16],Node [Leaf 17,Leaf 18,Leaf 19,Leaf 20]]]]
  print $ map (`lookup` l) ns
  -- [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
  print $ foldr (\i -> update i (i + 10)) l ns
  -- QL [[Leaf 10],
  --     [Node [Leaf 11,Leaf 12,Leaf 13,Leaf 14]],
  --     [Node [Node [Leaf 15,Leaf 16,Leaf 17,Leaf 18],Node [Leaf 19,Leaf 20,Leaf 21,Leaf 22],Node [Leaf 23,Leaf 24,Leaf 25,Leaf 26],Node [Leaf 27,Leaf 28,Leaf 29,Leaf 30]]]]
