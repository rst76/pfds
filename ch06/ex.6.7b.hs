import Data.List.Zipper hiding (empty)

data MergeSort a = MS Int [[a]] deriving Show

mrg :: Ord a => [a] -> [a] -> [a]
mrg [] ys = ys
mrg xs [] = xs
mrg xs@(x : xs') ys@(y : ys') =
  if x <= y then x : mrg xs' ys else y : mrg xs ys'

empty :: MergeSort a
empty = MS 0 []

add :: Ord a => a -> MergeSort a -> MergeSort a
add x (MS size segs) = MS (size + 1) (addSeg [x] segs size)
  where
  addSeg seg segs size = 
    if size `mod` 2 == 0 then seg : segs
    else addSeg (mrg seg (head segs)) (tail segs) (size `div` 2)

sort :: Ord a => MergeSort a -> [a]
sort (MS size segs) = foldl mrg [] segs

takeMin :: Ord a => Int -> MergeSort a -> [a]
takeMin n = take n . sort
-- To take the first element, log(n) times mrg operations are executed.
-- So to take k elements, it costs k*log(n)
-- takeMin 3 (MS 7 [[1], [2,3], [4,5,6,7]])
-- take 3 (foldl mrg [] [[1], [2,3], [4,5,6,7]])
-- take 3 (mrg (mrg (mrg [] [1]) [2,3]) [4,5,6,7])
-- take 3 (mrg (mrg [1] [2,3]) [4,5,6,7])
-- take 3 (mrg (1 : mrg [] [2,3]) [4,5,6,7])
-- take 3 (1 : mrg (mrg [] [2,3]) [4,5,6,7])
-- 1 : take 2 (mrg (mrg [] [2,3]) [4,5,6,7])
-- 1 : 2 : take 1 (mrg [3] [4,5,6,7])
-- 1 : 2 : 3 : take 0 [4,5,6,7]
-- 1 : 2 : 3 : []

-- Another version of takeMin (Zipper).
takeMin1 :: Ord a => Int -> MergeSort a -> [a]
takeMin1 n (MS size segs) = take' n (fromList segs)
  where
  take' n z =
    if n == 0 || emptyp z then []
    else x : take' (n - 1) z''
    where
    z' = foldlz minz z z
    x : xs = cursor z'
    z'' = if null xs then delete z' else replace xs z'
  minz a b = if head (cursor a) <= head (cursor b) then a else b

main :: IO ()
main = do
  let sortable = foldr add empty [1..7]
  -- => MS 7 [[1],[2,3],[4,5,6,7]]
  print $ takeMin 4 sortable
  -- => [1,2,3,4]
  print $ takeMin1 4 sortable
  -- => [1,2,3,4]
