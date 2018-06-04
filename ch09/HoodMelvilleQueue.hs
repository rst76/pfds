module HoodMelvilleQueue (HoodMelvilleQueue, lookup, update) where

import Prelude hiding (head, tail, lookup)
import Queue
import qualified RandomAccessList as RL
import SkewBinaryRandomAccessList (SkewList)

data RotationState a =
    Idle
  | Reversing Int Int (SkewList a) (SkewList a) (SkewList a) (SkewList a)
  | Appending Int Int (SkewList a) (SkewList a)
  | Done (SkewList a) deriving Show

data HoodMelvilleQueue a = HM Int (SkewList a) (RotationState a) Int (SkewList a) deriving Show

exec :: RotationState a -> RotationState a
exec (Reversing ok d f f' r r')
  | RL.isEmpty f = Appending ok d f' (RL.cons (RL.head r) r')
  | otherwise = Reversing (ok + 1) d (RL.tail f) (RL.cons (RL.head f) f') (RL.tail r) (RL.cons (RL.head r) r')
exec (Appending 0 _ _ r') = Done r'
exec (Appending ok d f' r') = Appending (ok - 1) d (RL.tail f') (RL.cons (RL.head f') r')
exec state = state

invalidate :: RotationState a -> RotationState a
invalidate (Reversing ok d f f' r r') = Reversing (ok - 1) (d + 1) f f' r r'
invalidate (Appending 0 _ _ r') = Done (RL.tail r')
invalidate (Appending ok d f' r') = Appending (ok - 1) (d + 1) f' r'
invalidate state = state

exec2 :: Int -> SkewList a -> RotationState a -> Int -> SkewList a -> HoodMelvilleQueue a
exec2 lenf f state lenr r = case exec state of
  Done newf -> HM lenf newf Idle lenr r
  newstate -> HM lenf f newstate lenr r

check :: Int -> SkewList a -> RotationState a -> Int -> SkewList a -> HoodMelvilleQueue a
check lenf f state lenr r =
  if lenr <= lenf then exec2 lenf f state lenr r
  else exec2 (lenf + lenr) f newstate 0 RL.empty
  where newstate = exec (Reversing 0 0 f RL.empty r RL.empty)

lookup :: Int -> HoodMelvilleQueue a -> a
lookup i (HM lenf _ _ lenr r)
  | i >= lenf = RL.lookup (lenf + lenr - i - 1) r
lookup i (HM _ f Idle _ _) = RL.lookup i f
lookup i (HM lenf f (Appending ok d _ r) _ _)
  | i < h     = RL.lookup i f
  | otherwise = RL.lookup (i - ok) r
  where
  h = (lenf - d) `div` 2
lookup i (HM lenf f (Reversing ok d _ _ rf rr) _ _)
  | i < h1    = RL.lookup i f
  | i < h2    = RL.lookup (h2 - i - 1) rf
  | otherwise = RL.lookup (i - h2) rr
  where
  h1 = (lenf - d) `div` 2
  h2 = lenf - d - ok

update :: Int -> a -> HoodMelvilleQueue a -> HoodMelvilleQueue a
update i x (HM lenf f state lenr r)
  | i >= lenf = HM lenf f state lenr (RL.update (lenf + lenr - i - 1) x r)
update i x (HM lenf f Idle lenr r) = HM lenf (RL.update i x f) Idle lenr r
update i x (HM lenf f (Appending ok d ff fr) lenr r)
  | i < ok    = HM lenf f' (Appending ok d ff' fr) lenr r
  | i < h     = HM lenf f' (Appending ok d ff fr') lenr r
  | otherwise = HM lenf f  (Appending ok d ff fr') lenr r
  where
  h = (lenf - d) `div` 2
  f'  = RL.update i x f
  ff' = RL.update (ok - i - 1) x ff
  fr' = RL.update (i - ok) x fr
update i x (HM lenf f (Reversing ok d fr ff rf rr) lenr r)
  | i < ok    = HM lenf f' (Reversing ok d fr ff' rf rr) lenr r
  | i < h1    = HM lenf f' (Reversing ok d fr' ff rf rr) lenr r
  | i < h2    = HM lenf f  (Reversing ok d fr ff rf' rr) lenr r
  | otherwise = HM lenf f  (Reversing ok d fr ff rf rr') lenr r
  where
  h1 = (lenf - d) `div` 2
  h2 = lenf - d - ok
  f'  = RL.update i x f
  ff' = RL.update (ok - i - 1) x ff
  fr' = RL.update (i - ok) x fr
  rf' = RL.update (h2 - i - 1) x rf
  rr' = RL.update (i - h2) x rr

instance Queue HoodMelvilleQueue where

  empty = HM 0 RL.empty Idle 0 RL.empty

  isEmpty (HM lenf _ _ _ _) = lenf == 0

  snoc (HM lenf f state lenr r) x = check lenf f state (lenr + 1) (RL.cons x r)

  head (HM _ f _ _ _) = RL.head f

  tail (HM lenf f state lenr r) = check (lenf - 1) (RL.tail f) (invalidate state) lenr r
