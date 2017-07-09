import Heap
import LeftistHeap
import ExplicitMinHeap

main :: IO ()
main = do
  print (foldr insert empty [1, 2, 3, 4, 5] :: ExplicitMinHeap LeftistHeap Int)
  print (foldr insert empty "becad"         :: ExplicitMinHeap LeftistHeap Char)
