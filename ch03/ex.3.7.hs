import Heap
import LeftistHeap
import BinomialHeap
import ExplicitMinHeap

main :: IO ()
main = do
  print (foldr insert empty [1, 2, 3, 4, 5, 6, 7] :: ExplicitMinHeap LeftistHeap  Int)
  -- => NE 1 (T 1 1 (T 1 2 (T 1 3 (T 1 4 (T 1 5 (T 1 6 (T 1 7 E E) E) E) E) E) E) E)
  print (foldr insert empty "ebdac"               :: ExplicitMinHeap LeftistHeap  Char)
  -- => NE 'a' (T 2 'a' (T 2 'b' (T 1 'd' E E) (T 1 'e' E E)) (T 1 'c' E E))
  print (foldr insert empty [1, 2, 3, 4, 5, 6, 7] :: ExplicitMinHeap BinomialHeap Int)
  -- => NE 1 (BH [Node 0 1 [],Node 1 2 [Node 0 3 []],Node 2 4 [Node 1 6 [Node 0 7 []],Node 0 5 []]])
  print (foldr insert empty "ebdac"               :: ExplicitMinHeap BinomialHeap Char)
  -- => NE 'a' (BH [Node 0 'e' [],Node 2 'a' [Node 1 'b' [Node 0 'd' []],Node 0 'c' []]])
