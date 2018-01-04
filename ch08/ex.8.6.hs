import Prelude hiding (head, tail)

type BankersDeque a = (Int, [a], Int, [a])

empty :: BankersDeque a
empty = (0, [], 0, [])

isEmpty :: BankersDeque a -> Bool
isEmpty (lenf, _, lenr, _) = lenf + lenr == 0

check :: BankersDeque a -> BankersDeque a
check q@(lenf, f, lenr, r)
  | lenf > 2 * lenr + 1 = (i, take i f, j, r ++ reverse (drop i f))
  | lenr > 2 * lenf + 1 = (j, f ++ reverse (drop i r), i, take i r)
  | otherwise = q
  where
  i = (lenf + lenr) `div` 2
  j = lenf + lenr - i

cons :: a -> BankersDeque a -> BankersDeque a
cons x (lenf, f, lenr, r) = check (lenf + 1, x : f, lenr, r)

head :: BankersDeque a -> a
head (_, [], _, []) = error "empty deque"
head (_, [], _, x : _) = x
head (_, x : _, _, _) = x

tail :: BankersDeque a -> BankersDeque a
tail (_, [], _, []) = error "empty deque"
tail (_, [], _, _ : _) = empty
tail (lenf, _ : f', lenr, r) = check (lenf - 1, f', lenr, r)

snoc :: BankersDeque a -> a -> BankersDeque a
snoc (lenf, f, lenr, r) x = check (lenf, f, lenr + 1, x : r)

last :: BankersDeque a -> a
last (_, [], _, []) = error "empty deque"
last (_, x : _, _, []) = x
last (_, _, _, x : _) = x

init :: BankersDeque a -> BankersDeque a
init (_, [], _, []) = error "empty deque"
init (_, _ : _, _, []) = empty
init (lenf, f, lenr, _ : r') = check (lenf, f, lenr - 1, r')

main :: IO ()
main = mapM_ print $ scanl snoc empty [1 .. 21]
