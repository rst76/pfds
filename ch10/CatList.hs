{-# LANGUAGE StandaloneDeriving, UndecidableInstances #-}

module CatList (CatList) where

import Prelude hiding (head, tail, (++))
import CatenableList
import qualified Queue as Q

data CatList q a = E | C a (q (CatList q a))
deriving instance (Show a, Show (q (CatList q a))) => Show (CatList q a)

link :: Q.Queue q => CatList q a -> CatList q a -> CatList q a
link (C x q) s = C x (Q.snoc q s)

flatten :: Q.Queue q => [CatList q a] -> CatList q a
flatten = foldr (++) E

instance Q.Queue q => CatenableList (CatList q) where

  empty = E

  isEmpty E = True
  isEmpty _ = False

  xs ++ E  = xs
  E  ++ xs = xs
  xs ++ ys = link xs ys

  cons x xs = C x Q.empty ++ xs

  snoc xs x = xs ++ C x Q.empty

  head E = error "empty list"
  head (C x q) = x

  tail E = error "empty list"
  tail (C x q) = if Q.isEmpty q then E else linkAII q
    where
    linkAII q = if Q.isEmpty q' then t else link t (linkAII q')
      where
      t = Q.head q
      q' = Q.tail q
