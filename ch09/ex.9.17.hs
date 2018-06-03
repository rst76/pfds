import Heap
import TrinomialHeap

main :: IO ()
main = mapM_ print $ take 10 $ iterate deleteMin (foldr insert empty [0 .. 8] :: TrinomialHeap Int)
  -- TH [Zero,Zero,One (Node 0 [(Node 3 [(Node 4 [],Node 5 [])],Node 6 [(Node 7 [],Node 8 [])]),(Node 1 [],Node 2 [])])]
  -- TH [Two (Node 1 []) (Node 2 []),Two (Node 3 [(Node 4 [],Node 5 [])]) (Node 6 [(Node 7 [],Node 8 [])])]
  -- TH [One (Node 2 []),Two (Node 3 [(Node 4 [],Node 5 [])]) (Node 6 [(Node 7 [],Node 8 [])])]
  -- TH [Zero,Two (Node 3 [(Node 4 [],Node 5 [])]) (Node 6 [(Node 7 [],Node 8 [])])]
  -- TH [Two (Node 4 []) (Node 5 []),One (Node 6 [(Node 7 [],Node 8 [])])]
  -- TH [One (Node 5 []),One (Node 6 [(Node 7 [],Node 8 [])])]
  -- TH [Zero,One (Node 6 [(Node 7 [],Node 8 [])])]
  -- TH [Two (Node 7 []) (Node 8 [])]
  -- TH [One (Node 8 [])]
  -- TH []
