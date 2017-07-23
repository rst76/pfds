insertionSort :: Ord a => [a] -> [a]
insertionSort = foldl ins []
  where
  ins [] x = [x]
  ins (y : ys) x
    | x < y = x : y : ys
    | otherwise = y : ins ys x

-- To detect each n numbers being the member of the first k numbers,
-- we have to compare at most to the first k members of the sorted list.
-- (It's not necessary to compare to the rest members of the sorted list.)
-- So, sort xs takes O(n * k) time.
