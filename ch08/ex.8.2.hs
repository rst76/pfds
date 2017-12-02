import Prelude hiding (head, tail)

data RotationState a =
    Idle
  | Reversing Int [a] [a] [a] [a]
  | Appending Int [a] [a]
  | Done [a] deriving Show

data HoodMelvilleQueue a = HM Int [a] (RotationState a) Int [a] deriving Show

exec :: RotationState a -> RotationState a
exec (Reversing ok (x : f) f' (y : r) r') = Reversing (ok + 1) f (x : f') r (y : r')
exec (Reversing ok [] f' [y] r') = Appending ok f' (y : r')
exec (Appending 0 _ r') = Done r'
exec (Appending ok (x : f') r') = Appending (ok - 1) f' (x : r')
exec state = state

invalidate :: RotationState a -> RotationState a
invalidate (Reversing ok f f' r r') = Reversing (ok - 1) f f' r r'
invalidate (Appending 0 _ (_ : r')) = Done r'
invalidate (Appending ok f' r') = Appending (ok - 1) f' r'
invalidate state = state

exec2 :: Int -> [a] -> RotationState a -> Int -> [a] -> HoodMelvilleQueue a
exec2 lenf f state lenr r = case exec state of
  Done newf -> HM lenf newf Idle lenr r
  newstate -> HM lenf f newstate lenr r

check :: Int -> [a] -> RotationState a -> Int -> [a] -> HoodMelvilleQueue a
check lenf f state lenr r =
  if lenr <= lenf then exec2 lenf f state lenr r
  else exec2 (lenf + lenr) f newstate 0 []
  where newstate = exec (Reversing 0 f [] r [])

empty :: HoodMelvilleQueue a
empty = HM 0 [] Idle 0 []

snoc :: HoodMelvilleQueue a -> a -> HoodMelvilleQueue a
snoc (HM lenf f state lenr r) x = check lenf f state (lenr + 1) (x : r)

head :: HoodMelvilleQueue a -> a
head (HM _ [] _ _ _) = error "empty queue"
head (HM _ (x : _) _ _ _) = x

tail :: HoodMelvilleQueue a -> HoodMelvilleQueue a
tail (HM _ [] _ _ _) = error "empty queue"
tail (HM lenf (_ : f') state lenr r) = check (lenf - 1) f' (invalidate state) lenr r

main :: IO ()
main = do
  let qs = scanl snoc empty [1 .. 7]
  mapM_ print qs
  -- => HM 0 [] Idle 0 []
  -- => HM 1 [1] Idle 0 []
  -- => HM 1 [1] Idle 1 [2]
  -- => HM 3 [1] (Appending 1 [1] [2,3]) 0 []
  -- => HM 3 [1] (Appending 0 [] [1,2,3]) 1 [4]
  -- => HM 3 [1,2,3] Idle 2 [5,4]
  -- => HM 3 [1,2,3] Idle 3 [6,5,4]
  -- => HM 7 [1,2,3] (Reversing 2 [3] [2,1] [5,4] [6,7]) 0 []
  mapM_ print $ take 8 $ iterate tail $ last qs
  -- => HM 7 [1,2,3] (Reversing 2 [3] [2,1] [5,4] [6,7]) 0 []
  -- => HM 6 [2,3] (Reversing 2 [] [3,2,1] [4] [5,6,7]) 0 []
  -- => HM 5 [3] (Appending 1 [3,2,1] [4,5,6,7]) 0 []
  -- => HM 4 [4,5,6,7] Idle 0 []
  -- => HM 3 [5,6,7] Idle 0 []
  -- => HM 2 [6,7] Idle 0 []
  -- => HM 1 [7] Idle 0 []
  -- => HM 0 [] Idle 0 []
