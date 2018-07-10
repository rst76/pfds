import Prelude hiding (head, tail)
import Queue
import AltBootstrappedQueue2
import HoodMelvilleQueue

main :: IO ()
main = do
  let q = foldl snoc empty [0 .. 9] :: BootstrappedQueue HoodMelvilleQueue Int
  print q
  -- Q 7 [0] (HM 1 [[1,2]] Idle 1 [[3,4,5,6]]) 3 [9,8,7]
  print $ take 10 $ map head $ iterate tail q
  -- [0,1,2,3,4,5,6,7,8,9]
