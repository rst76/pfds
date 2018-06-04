import Prelude hiding (tail, lookup)
import Queue
import HoodMelvilleQueue

main :: IO ()
main = do
  let q1 = tail $ tail $ foldl snoc empty [0 .. 13] :: HoodMelvilleQueue Int
  print q1
  -- HM 12 (SL [(1,Leaf 2),(1,Leaf 3),(3,Node 4 (Leaf 5) (Leaf 6))]) (Reversing 2 1 (SL [(3,Node 4 (Leaf 5) (Leaf 6))]) (SL [(3,Node 3 (Leaf 2) (Leaf 1))]) (SL [(1,Leaf 10),(3,Node 9 (Leaf 8) (Leaf 7))]) (SL [(3,Node 11 (Leaf 12) (Leaf 13))])) 0 (SL [])
  print $ map (`lookup` q1) [0 .. 11]
  -- [2,3,4,5,6,7,8,9,10,11,12,13]
  print $ foldr (\i -> update i (i + 10)) q1 [0 .. 11]
  -- HM 12 (SL [(1,Leaf 10),(1,Leaf 11),(3,Node 12 (Leaf 13) (Leaf 14))]) (Reversing 2 1 (SL [(3,Node 12 (Leaf 13) (Leaf 14))]) (SL [(3,Node 11 (Leaf 10) (Leaf 1))]) (SL [(1,Leaf 18),(3,Node 17 (Leaf 16) (Leaf 15))]) (SL [(3,Node 19 (Leaf 20) (Leaf 21))])) 0 (SL [])
  let q2 = tail $ tail q1 :: HoodMelvilleQueue Int
  print q2
  -- HM 10 (SL [(3,Node 4 (Leaf 5) (Leaf 6))]) (Reversing 2 3 (SL [(1,Leaf 6)]) (SL [(1,Leaf 5),(1,Leaf 4),(3,Node 3 (Leaf 2) (Leaf 1))]) (SL [(1,Leaf 8),(1,Leaf 7)]) (SL [(1,Leaf 9),(1,Leaf 10),(3,Node 11 (Leaf 12) (Leaf 13))])) 0 (SL [])
  print $ map (`lookup` q2) [0 .. 9]
  -- [4,5,6,7,8,9,10,11,12,13]
  print $ foldr (\i -> update i (i + 10)) q2 [0 .. 9]
  -- HM 10 (SL [(3,Node 10 (Leaf 11) (Leaf 12))]) (Reversing 2 3 (SL [(1,Leaf 12)]) (SL [(1,Leaf 11),(1,Leaf 10),(3,Node 3 (Leaf 2) (Leaf 1))]) (SL [(1,Leaf 14),(1,Leaf 13)]) (SL [(1,Leaf 15),(1,Leaf 16),(3,Node 17 (Leaf 18) (Leaf 19))])) 0 (SL [])
  let q3 = tail $ tail q2 :: HoodMelvilleQueue Int
  print q3
  -- HM 8 (SL [(1,Leaf 6)]) (Appending 1 5 (SL [(3,Node 6 (Leaf 5) (Leaf 4)),(3,Node 3 (Leaf 2) (Leaf 1))]) (SL [(7,Node 7 (Node 8 (Leaf 9) (Leaf 10)) (Node 11 (Leaf 12) (Leaf 13)))])) 0 (SL [])
  print $ map (`lookup` q3) [0 .. 7]
  -- [6,7,8,9,10,11,12,13]
  print $ foldr (\i -> update i (i + 10)) q3 [0 .. 7]
  -- HM 8 (SL [(1,Leaf 10)]) (Appending 1 5 (SL [(3,Node 10 (Leaf 5) (Leaf 4)),(3,Node 3 (Leaf 2) (Leaf 1))]) (SL [(7,Node 11 (Node 12 (Leaf 13) (Leaf 14)) (Node 15 (Leaf 16) (Leaf 17)))])) 0 (SL [])
