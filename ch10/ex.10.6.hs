import Prelude hiding (head, tail, (++))
import SimpleQueue
import CatenableList
import CatList

main :: IO ()
main = do
  let qs = map (snoc empty) [0 .. 5] :: [CatList SimpleQueue Int]
  print $ foldl (++) empty qs
  -- C 0 (SQ [C 1 (SQ []),C 2 (SQ []),C 3 (SQ []),C 4 (SQ []),C 5 (SQ [])])
  print $ foldr (++) empty qs
  -- C 0 (SQ [C 1 (SQ [C 2 (SQ [C 3 (SQ [C 4 (SQ [C 5 (SQ [])])])])])])
