import Heap
import SkewBinomialHeap
import DeletableHeap

main :: IO ()
main = do
  let h = delete 3 $ delete 2 $ foldr insert empty [1 .. 5] :: DeletableHeap SkewBinomialHeap Int
  print h
  -- DH (SBH [Node 0 3 [] [],Node 0 2 [] []]) (SBH [Node 0 1 [] [],Node 0 2 [] [],Node 1 3 [4] [Node 0 5 [] []]])
  print $ findMin h
  -- 1
  print $ deleteMin h
  -- DH (SBH []) (SBH [Node 0 4 [] [],Node 0 5 [] []])
  print $ findMin $ deleteMin h 
  -- 4
