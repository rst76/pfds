import Prelude hiding (tail, lookup)
import Queue
import HoodMelvilleQueue

main :: IO ()
main = do
  let ns1 = [0 .. 6]
  let q1 = foldl snoc empty ns1 :: HoodMelvilleQueue Int
  print q1
  -- HM 7 (SL [(3,Node 0 (Leaf 1) (Leaf 2))]) (Reversing 2 (SL [(1,Leaf 2)]) (SL [(1,Leaf 1),(1,Leaf 0)]) (SL [(1,Leaf 4),(1,Leaf 3)]) (SL [(1,Leaf 5),(1,Leaf 6)])) 0 (SL [])
  print $ map (`lookup` q1) ns1
  -- [0,1,2,3,4,5,6]
  print $ foldl (\q i -> update i (i + 10) q) q1 ns1
  -- HM 7 (SL [(3,Node 10 (Leaf 11) (Leaf 12))]) (Reversing 2 (SL [(1,Leaf 12)]) (SL [(1,Leaf 11),(1,Leaf 10)]) (SL [(1,Leaf 14),(1,Leaf 13)]) (SL [(1,Leaf 15),(1,Leaf 16)])) 0 (SL [])
  let ns2 = [0 .. 9]
  let q2 = foldl snoc empty ns2 :: HoodMelvilleQueue Int
  print q2
  -- HM 7 (SL [(3,Node 0 (Leaf 1) (Leaf 2))]) (Appending 2 (SL [(1,Leaf 1),(1,Leaf 0)]) (SL [(1,Leaf 2),(1,Leaf 3),(3,Node 4 (Leaf 5) (Leaf 6))])) 3 (SL [(3,Node 9 (Leaf 8) (Leaf 7))])
  print $ map (`lookup` q2) ns2
  -- [0,1,2,3,4,5,6,7,8,9]
  print $ foldl (\q i -> update i (i + 10) q) q2 ns2
  -- HM 7 (SL [(3,Node 10 (Leaf 11) (Leaf 12))]) (Appending 2 (SL [(1,Leaf 11),(1,Leaf 10)]) (SL [(1,Leaf 12),(1,Leaf 13),(3,Node 14 (Leaf 15) (Leaf 16))])) 3 (SL [(3,Node 19 (Leaf 18) (Leaf 17))])