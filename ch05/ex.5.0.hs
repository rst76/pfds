type Queue a = ([a], [a])

empty :: Queue a
empty = ([], [])

check :: [a] -> [a] -> Queue a
check [] r = (reverse r, [])
check f  r = (f, r)

snoc :: Queue a -> a -> Queue a
snoc (f, r) x = check f (x : r)

head :: Queue a -> a
head ([], _) = error "empty queue"
head (x : _, _) = x

tail :: Queue a -> Queue a
tail ([], _) = error "empty queue"
tail (_ : f, r) = check f r
