module DList
  ( DList
  , toDList
  , fromDList
  ) where

import Data.Semigroup (Semigroup((<>)))

newtype DList a = DList ([a] -> [a])

instance Semigroup (DList a) where
  DList f <> DList g = DList $ f <> g

instance Monoid (DList a) where
  mempty = DList id
  mappend = (<>)

toDList :: [a] -> DList a
toDList xs = DList (xs <>)

fromDList :: DList a -> [a]
fromDList (DList f) = f []
