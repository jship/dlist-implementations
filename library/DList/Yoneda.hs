{-# LANGUAGE Rank2Types #-}

module DList.Yoneda
  ( DList
  , toDList
  , fromDList
  ) where

import Control.Applicative (Const(Const, getConst))
import Data.Semigroup (Semigroup((<>)))

newtype DList a = DList (Yoneda (Const [a]) ())

instance Semigroup (DList a) where
  DList (Yoneda f) <> DList (Yoneda g) = DList (Yoneda $ f <> g)

instance Monoid (DList a) where
  mempty = DList (Yoneda $ const mempty)
  mappend = (<>)

toDList :: [a] -> DList a
toDList xs = DList (toYoneda (Const xs))

fromDList :: DList a -> [a]
fromDList (DList yo) = getConst . fromYoneda $ yo

------------------------------------------------------------------------------

newtype Yoneda f a = Yoneda (forall b. (a -> b) -> f b)

toYoneda :: Functor f => f a -> Yoneda f a
toYoneda fa = Yoneda $ flip fmap fa

fromYoneda :: Yoneda f a -> f a
fromYoneda (Yoneda f) = f id
