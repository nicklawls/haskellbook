module MaybeT where

import           Control.Applicative
import           Control.Monad

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}


instance Functor f => Functor (MaybeT f) where
  fmap f (MaybeT mma) = MaybeT $ (fmap . fmap) f mma


instance Applicative f => Applicative (MaybeT f) where
  pure = MaybeT . pure . pure
  (<*>) (MaybeT mmf) (MaybeT mma) = MaybeT $ liftA2 (<*>) mmf mma


instance Monad m => Monad (MaybeT m) where
  return = pure
  (>>=) (MaybeT mma) f = MaybeT $
    mma >>= maybe (return Nothing) (runMaybeT . f)

