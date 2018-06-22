module DList.Endo
  ( DList
  , toDList
  , fromDList
  ) where

import Data.Monoid (Endo(Endo, appEndo))
import Data.Semigroup (Semigroup((<>)))

newtype DList a = DList (Endo [a])

instance Semigroup (DList a) where
  DList f <> DList g = DList $ f <> g

instance Monoid (DList a) where
  mempty = DList mempty
  mappend = (<>)

toDList :: [a] -> DList a
toDList xs = DList (Endo (xs <>))

fromDList :: DList a -> [a]
fromDList (DList endo) = appEndo endo []
