module MonadWorkout where

data Nope a = NopeDotJpg


instance Functor Nope where
  fmap _ _ = NopeDotJpg


instance Applicative Nope where
  pure = const NopeDotJpg
  _ <*> _ = NopeDotJpg


instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg


data Identity a =
  Identity a
  deriving (Eq, Show, Ord)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)


instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)


instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a


data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show)


instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a rest) = Cons (f a) (fmap f rest)


append :: List a -> List a -> List a
append (Cons a as) rest = Cons a (append as rest)
append Nil rest = rest

instance Applicative List where
  pure x = Cons x Nil
  Nil         <*> _    = Nil
  -- (Cons _ _)  <*> Nil  = Nil
  (Cons f fs) <*> rest = append (fmap f rest) (fs <*> rest)

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  (Cons x xs) >>= f = append (f x) (xs >>= f)


j :: Monad m => m (m a) -> m a
j burritoBurrito =
  burritoBurrito >>= id


l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap


l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma ba = f <$> ma <*> ba


-- a :: Monad m => m a -> m (a -> b) -> m b
-- a = flip (<*>)

-- reconstruct the list inside the monad context
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (a:as) f = f a >>= (\b -> fmap (b:) (meh as f))

-- rconstruct the list inside the appliciative context
meh' :: Applicative f => (a -> f b) -> [a] -> f [b]
-- meh' _ [] = pure []
meh' f = foldr (\a acc -> (:) <$> f a <*> acc) (pure [])


flipType :: Monad m => [m a] -> m [a]
flipType mas = meh mas id