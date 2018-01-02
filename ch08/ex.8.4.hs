import Prelude hiding (head, tail)
import HoodMelvilleQueue (HoodMelvilleQueue)
import OutputRestrictedDeque (OutputRestrictedDeque, cons)
import Queue

main :: IO ()
main = do
  let q0 = empty :: OutputRestrictedDeque HoodMelvilleQueue Int
  let q1 = snoc (snoc (cons 1 q0) 2) 3
  print q1
  -- => ORD [1] (HM 1 [2] Idle 1 [3])
  let q2 = tail q1
  print q2
  -- => ORD [] (HM 1 [2] Idle 1 [3])
  print $ isEmpty q2
  -- => False
  let q3 = tail q2
  print q3
  -- => ORD [] (HM 1 [3] Idle 0 [])
  print $ isEmpty q3
  -- => False
  let q4 = tail q3
  print q4
  -- => ORD [] (HM 0 [] Idle 0 [])
  print $ isEmpty q4
  -- => True
