import Prelude hiding (head, tail, last, init)

class Deque q where

  empty :: q a
  isEmpty :: q a -> Bool

  cons :: a -> q a -> q a
  head :: q a -> a
  tail :: q a -> q a

  snoc :: q a -> a -> q a
  last :: q a -> a
  init :: q a -> q a

data BatchedDeque a = BD [a] [a] deriving Show

checkf :: BatchedDeque a -> BatchedDeque a
checkf (BD [] r) = BD (reverse b) a
  where
  (a, b) = splitAt ((length r + 1) `div` 2) r
checkf q = q

checkr :: BatchedDeque a -> BatchedDeque a
checkr (BD f []) = BD a (reverse b)
  where
  (a, b) = splitAt ((length f + 1) `div` 2) f
checkr q = q

instance Deque BatchedDeque where

  empty = BD [] []

  isEmpty (BD [] []) = True
  isEmpty _ = False

  cons x (BD f r) = checkr (BD (x : f) r)

  head (BD [] []) = error "empty deque"
  head (BD [] [x]) = x
  head (BD (x : _) _) = x

  tail (BD [] []) = error "empty deque"
  tail (BD [] [_]) = BD [] []
  tail (BD (_ : f) r) = checkf (BD f r)

  snoc (BD f r) x = checkf (BD f (x : r))

  last (BD [] []) = error "empty deque"
  last (BD [x] []) = x
  last (BD _ (x : _)) = x

  init (BD [] []) = error "empty deque"
  init (BD [_] []) = BD [] []
  init (BD f (_ : r)) = checkr (BD f r)

main :: IO ()
main = do
  mapM_ print (take 6 $ iterate tail $ foldl snoc empty [1,2,3,4,5] :: [BatchedDeque Int])
  -- => BD [1] [5,4,3,2]
  -- => BD [2,3] [5,4]
  -- => BD [3] [5,4]
  -- => BD [4] [5]
  -- => BD [] [5]
  -- => BD [] []
  mapM_ print (take 6 $ iterate init $ foldl snoc empty [1,2,3,4,5] :: [BatchedDeque Int])
  -- => BD [1] [5,4,3,2]
  -- => BD [1] [4,3,2]
  -- => BD [1] [3,2]
  -- => BD [1] [2]
  -- => BD [1] []
  -- => BD [] []
  mapM_ print (take 6 $ iterate init $ foldr cons empty [1,2,3,4,5] :: [BatchedDeque Int])
  -- => BD [1,2,3,4] [5]
  -- => BD [1,2] [4,3]
  -- => BD [1,2] [3]
  -- => BD [1] [2]
  -- => BD [1] []
  -- => BD [] []
  mapM_ print (take 6 $ iterate tail $ foldr cons empty [1,2,3,4,5] :: [BatchedDeque Int])
  -- => BD [1,2,3,4] [5]
  -- => BD [2,3,4] [5]
  -- => BD [3,4] [5]
  -- => BD [4] [5]
  -- => BD [] [5]
  -- => BD [] []
