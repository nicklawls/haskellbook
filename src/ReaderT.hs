module ReaderT
    (
    ) where

import           Control.Applicative
import           Control.Monad

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a}

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $
        (fmap . fmap) f rma
--        ^       ^
--        |       |
--              apply f inside the monad
--        compose f with the reader function

instance Applicative m => Applicative (ReaderT r m) where
  pure = ReaderT . pure . pure
  (ReaderT rmab) <*> (ReaderT rma) =
    ReaderT $ liftA2 (<*>) rmab rma


instance Monad m => Monad (ReaderT r m) where
  return = pure
  (ReaderT rma) >>= artmb =
    ReaderT $ \r ->
      rma r >>= (\a -> runReaderT (artmb a) r)

