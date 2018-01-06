import Prelude hiding (head, tail, last, init)

type BankersDeque a = (Int, [a], Int, [a])

empty :: BankersDeque a
empty = (0, [], 0, [])

isEmpty :: BankersDeque a -> Bool
isEmpty (lenf, _, lenr, _) = lenf + lenr == 0

check :: BankersDeque a -> Int -> BankersDeque a
check q@(lenf, f, lenr, r) c
  | lenf > c * lenr + 1 = (i, take i f, j, r ++ reverse (drop i f))
  | lenr > c * lenf + 1 = (j, f ++ reverse (drop i r), i, take i r)
  | otherwise = q
  where
  i = (lenf + lenr) `div` 2
  j = lenf + lenr - i

cons :: a -> BankersDeque a -> Int -> BankersDeque a
cons x (lenf, f, lenr, r) = check (lenf + 1, x : f, lenr, r)

head :: BankersDeque a -> a
head (_, [], _, []) = error "empty deque"
head (_, [], _, x : _) = x
head (_, x : _, _, _) = x

tail :: BankersDeque a -> Int -> BankersDeque a
tail (_, [], _, []) = error "empty deque"
tail (_, [], _, _ : _) = pure empty
tail (lenf, _ : f', lenr, r) = check (lenf - 1, f', lenr, r)

snoc :: BankersDeque a -> a -> Int -> BankersDeque a
snoc (lenf, f, lenr, r) x = check (lenf, f, lenr + 1, x : r)

last :: BankersDeque a -> a
last (_, [], _, []) = error "empty deque"
last (_, x : _, _, []) = x
last (_, _, _, x : _) = x

init :: BankersDeque a -> Int -> BankersDeque a
init (_, [], _, []) = error "empty deque"
init (_, _ : _, _, []) = pure empty
init (lenf, f, lenr, _ : r') = check (lenf, f, lenr - 1, r')

main :: IO ()
main = do
  let q20 = (1, [0], 1, [0])
  let q40 = q20

  let q21 = iterate (\q -> snoc q 0 2) q20 !! 3
  -- => (3,[0,0,0],2,[0,0]) : 1 rotation
  let q41 = iterate (\q -> snoc q 0 4) q40 !! 3
  -- => (1,[0],4,[0,0,0,0]) : 0 rotation
  let q22 = iterate (\q -> cons 0 q 2) q21 !! 3
  -- => (4,[0,0,0,0],4,[0,0,0,0]) : 1 rotation
  let q42 = iterate (\q -> cons 0 q 4) q41 !! 3
  -- => (4,[0,0,0,0],4,[0,0,0,0]) : 0 rotation
  let q23 = iterate (\q -> snoc q 0 2) q22 !! 6
  -- => (7,[0 .. 0],7,[0 .. 0]) : 1 rotation
  let q43 = iterate (\q -> snoc q 0 4) q42 !! 6
  -- => (4,[0 .. 0],10,[0 .. 0]) : 0 rotation
  let q24 = iterate (\q -> cons 0 q 2) q23 !! 9
  -- => (11,[0 .. 0],12,[0 .. 0]) : 1 rotation
  let q44 = iterate (\q -> cons 0 q 4) q43 !! 9
  -- => (13,[0 .. 0],10,[0 .. 0]) : 0 rotation

  let q40 = (1000, replicate 1000 0, 1000, replicate 1000 0)
  let q20 = q40
  let q41 = iterate (`tail` 4) q40 !!  751
  -- => (625,[0 .. 0],624,[0 .. 0]) : 1 rotation
  let q21 = iterate (`tail` 2) q20 !!  751
  -- => (500,[0 .. 0],749,[0 .. 0]) : 1 rotation
  let q42 = iterate (`init` 4) q41 !!  469
  -- => (390,[0 .. 0],390,[0 .. 0]) : 1 rotation
  let q22 = iterate (`init` 2) q21 !!  469
  -- => (500,[0 .. 0],280,[0 .. 0]) : 0 rotation
  let q43 = iterate (`tail` 4) q42 !!  293
  -- => (244,[0 .. 0],243,[0 .. 0]) : 1 rotation
  let q23 = iterate (`tail` 2) q22 !!  293
  -- => (207,[0 .. 0],280,[0 .. 0]) : 0 rotation
  let q44 = iterate (`init` 4) q43 !!  183
  -- => (152,[0 .. 0],152,[0 .. 0]) : 1 rotation
  let q24 = iterate (`init` 2) q23 !!  183
  -- => (207,[0 .. 0],97,[0 .. 0]) : 0 rotation
  return ()
