{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Twinplicative where

import           Control.Applicative
import           Data.Foldable

newtype Compose f g a =
  Compose { getCompose :: f (g a) }

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ fmap (fmap f) fga



instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose (pure (pure a))

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose a) = Compose $ liftA2 (<*>) f a

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap am (Compose fga) = (foldMap . foldMap) am $ fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose fga) =
    Compose <$> (traverse (traverse f) fga)


