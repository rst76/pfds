import Heap
import BinomialHeap
import SizedHeap

main :: IO ()
main = do
  print (foldr insert empty [1, 2, 3, 4, 5, 6, 7] :: SizedHeap BinomialHeap Int)
  -- => SH 7 (BH [Node 0 1 [],Node 1 2 [Node 0 3 []],Node 2 4 [Node 1 6 [Node 0 7 []],Node 0 5 []]])
  print (foldr insert empty "ebdac"               :: SizedHeap BinomialHeap Char)
  -- => SH 5 (BH [Node 0 'e' [],Node 2 'a' [Node 1 'b' [Node 0 'd' []],Node 0 'c' []]])
