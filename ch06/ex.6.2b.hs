import Prelude hiding (head, tail)

type Queue a = (Int, [a], Int, [a])

empty :: Queue a
empty = (0, [], 0, [])

isEmpty :: Queue a -> Bool
isEmpty (lenf, _, _, _) = lenf == 0

check :: Int -> Queue a -> Queue a
check n q@(lenf, f, lenr, r) =
  if lenr <= n * lenf then q else (lenf + lenr, f ++ reverse r, 0, [])

snoc :: Int -> Queue a -> a -> Queue a
snoc n (lenf, f, lenr, r) x = check n (lenf, f, lenr + 1, x : r)

head :: Queue a -> a
head (_, [], _, _) = error "empty"
head (_, x : _, _, _) = x

tail :: Int -> Queue a -> Queue a
tail _ (_, [], _, _) = error "empty"
tail n (lenf, _ : f, lenr, r) = check n (lenf - 1, f, lenr, r)

main :: IO ()
main = do
  let xs1 = take 101 (iterate (flip (snoc 1) 1) empty)
  mapM_ print $ init xs1 ++ take 101 (iterate (tail 1) (last xs1))
  let xs2 = take 101 (iterate (flip (snoc 2) 2) empty)
  mapM_ print $ init xs2 ++ take 101 (iterate (tail 2) (last xs2))

-- Cost(|f|>=|r|): 1 + 3 + 7 + 15 + 31 + 63 + 73 = 193
-- (0,[],0,[])
-- (1,[1],0,[])
-- (1,[1],1,[1])
-- (3,[1,1,1],0,[])
-- (3,[1,1,1],1,[1])
-- (3,[1,1,1],2,[1,1])
-- (3,[1,1,1],3,[1,1,1])
-- (7,[1,1,1,1,1,1,1],0,[])
-- :
-- (7,[1..1],7,[1..1])
-- (15,[1..1],0,[])
-- :
-- (15,[1..1],15,[1..1])
-- (31,[1..1],0,[])
-- :
-- (31,[1..1],31,[1..1])
-- (63,[1..1],0,[])
-- :
-- (63,[1..1],37,[1..1])
-- (62,[1..1],37,[1..1])
-- :
-- (37,[1..1],37,[1..1])
-- (73,[1..1],0,[])
-- :
-- (0,[],0,[])

-- Cost(2|f|>=|r|): 1 + 4 + 13 + 40 + 89 = 147
-- (0,[],0,[])
-- (1,[2],0,[])
-- (1,[2],1,[2])
-- (1,[2],2,[2,2])
-- (4,[2,2,2,2],0,[])
-- :
-- (4,[2,2,2,2],8,[2,2,2,2,2,2,2,2])
-- (13,[2..2],0,[])
-- :
-- (13,[2..2],26,[2..2])
-- (40,[2..2],0,[])
-- :
-- (40,[2..2],60,[2..2])
-- (39,[2..2],60,[2..2])
-- :
-- (30,[2..2],60,[2..2])
-- (89,[2..2],0,[])
-- :
-- (0,[],0,[])
