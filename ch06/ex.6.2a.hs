type Queue a = (Int, [a], Int, [a])

empty :: Queue a
empty = (0, [], 0, [])

check :: Queue a -> Queue a
check q@(lenf, f, lenr, r) =
  if lenr <= lenf then q
  else (lenf + lenr, f ++ reverse r, 0, [])

snoc :: Queue a -> a -> Queue a
snoc (lenf, f, lenr, r) x = check (lenf, f, lenr + 1, x : r)

head :: Queue a -> a
head (_, [], _, _) = error "empty queue"
head (_, x : _, _, _) = x

tail :: Queue a -> Queue a
tail (_, [], _, _) = error "empty queue"
tail (lenf, _ : f, lenr, r) = check (lenf - 1, f, lenr, r)
