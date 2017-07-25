insertionSort :: Ord a => [a] -> [a]
insertionSort = foldl ins []
  where
  ins [] x = [x]
  ins (y : ys) x
    | x < y = x : y : ys
    | otherwise = y : ins ys x
