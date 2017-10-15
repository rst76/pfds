import Prelude hiding (head, tail)
import qualified Prelude (tail)

data PhysisistsQueue a = PQ [a] Int [a] Int [a]

check :: [a] -> Int -> [a] -> Int -> [a] -> PhysisistsQueue a
check w lenf f lenr r =
  if lenr <= lenf then checkw w lenf f lenr r
  else checkw f (lenf + lenr) (f ++ reverse r) 0 []

checkw :: [a] -> Int -> [a] -> Int -> [a] -> PhysisistsQueue a
checkw [] lenf f lenr r = PQ f lenf f lenr r
checkw w lenf f lenr r = PQ w lenf f lenr r

empty :: PhysisistsQueue a
empty = PQ [] 0 [] 0 []

isEmpty :: PhysisistsQueue a -> Bool
isEmpty (PQ _ lenf _ _ _) = lenf == 0

snoc :: PhysisistsQueue a -> a -> PhysisistsQueue a
snoc (PQ w lenf f lenr r) x = check w lenf f (lenr + 1) (x : r)

head :: PhysisistsQueue a -> a
head (PQ [] _ _ _ _) = error "empty queue"
head (PQ (x : _) _ _ _ _) = x

tail :: PhysisistsQueue a -> PhysisistsQueue a
tail (PQ [] _ _ _ _) = error "empty queue"
tail (PQ (_ : w) lenf f lenr r) = check w (lenf - 1) (Prelude.tail f) lenr r
