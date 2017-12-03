import Prelude hiding (head, tail)

data RotationState a =
    Idle
  | Reversing Int [a] [a] [a] [a]
  | Appending Int [a] [a]
  | Done [a] deriving Show

data HoodMelvilleQueue a = HM [a] Int (RotationState a) [a] deriving Show

exec :: Int -> RotationState a -> (Int, RotationState a)
exec diff (Reversing ok (x : f) f' (y : r) r') = (diff + 2, Reversing (ok + 1) f (x : f') r (y : r'))
exec diff (Reversing ok []      f' [y]     r') = (diff + 1, Appending  ok             f'    (y : r'))
exec diff (Appending 0          _          r') = (diff,     Done r')
exec diff (Appending ok    (x : f')        r') = (diff,     Appending (ok - 1)        f'    (x : r'))
exec diff state = (diff, state)

invalidate :: RotationState a -> RotationState a
invalidate (Reversing ok f f' r r') = Reversing (ok - 1) f f' r r'
invalidate (Appending 0 _ (_ : r')) = Done r'
invalidate (Appending ok   f'   r') = Appending (ok - 1)   f'   r'
invalidate state = state

exec2 :: [a] -> Int -> RotationState a -> [a] -> HoodMelvilleQueue a
exec2 f diff state r = case exec diff state of
  (newdiff, Done newf) -> HM newf newdiff Idle r
  (newdiff, newstate) -> HM f newdiff newstate r

check :: [a] -> Int -> RotationState a -> [a] -> HoodMelvilleQueue a
check f diff state r =
  if diff >= 0 then exec2 f diff state r
  else exec2 f newdiff newstate []
  where (newdiff, newstate) = exec 0 (Reversing 0 f [] r [])

empty :: HoodMelvilleQueue a
empty = HM [] 0 Idle []

snoc :: HoodMelvilleQueue a -> a -> HoodMelvilleQueue a
snoc (HM f diff state r) x = check f (diff - 1) state (x : r)

head :: HoodMelvilleQueue a -> a
head (HM [] _ _ _) = error "empty queue"
head (HM (x : _) _ _ _) = x

tail :: HoodMelvilleQueue a -> HoodMelvilleQueue a
tail (HM [] _ _ _) = error "empty queue"
tail (HM (_ : f') diff state r) = check f' (diff - 1) (invalidate state) r

main :: IO ()
main = do
  let qs = scanl snoc empty [1 .. 7]
  mapM_ print qs
  -- => HM [] 0 Idle []
  -- => HM [1] 1 Idle []
  -- => HM [1] 0 Idle [2]
  -- => HM [1] 3 (Appending 1 [1] [2,3]) []
  -- => HM [1] 2 (Appending 0 [] [1,2,3]) [4]
  -- => HM [1,2,3] 1 Idle [5,4]
  -- => HM [1,2,3] 0 Idle [6,5,4]
  -- => HM [1,2,3] 4 (Reversing 2 [3] [2,1] [5,4] [6,7]) []
  mapM_ print $ take 8 $ iterate tail $ last qs
  -- => HM [1,2,3] 4 (Reversing 2 [3] [2,1] [5,4] [6,7]) []
  -- => HM [2,3] 5 (Reversing 2 [] [3,2,1] [4] [5,6,7]) []
  -- => HM [3] 5 (Appending 1 [3,2,1] [4,5,6,7]) []
  -- => HM [4,5,6,7] 4 Idle []
  -- => HM [5,6,7] 3 Idle []
  -- => HM [6,7] 2 Idle []
  -- => HM [7] 1 Idle []
  -- => HM [] 0 Idle []
