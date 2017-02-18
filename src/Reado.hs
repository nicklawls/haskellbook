{-# LANGUAGE InstanceSigs #-}
module Reado where

import           Control.Applicative
import           Data.Char

boop :: Num a => a -> a
boop = (*2)

doop :: Num a => a -> a
doop = (+10)

bip :: Num a => a -> a
bip = boop . doop

bloop :: Integer -> Integer
bloop = fmap boop doop


cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed = cap . rev

fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev
-- <$> composes (,) and cap, creating a function returning a tuple whose first value is the first argument applied to cap, and whose second value is the second argument
-- <*> :: (a -> a -> b) -> (a -> a) -> (a -> b)
-- <*> f g = \a -> f a (g a)

tupledM :: [Char] -> ([Char], [Char])
tupledM = do
  a <- cap  -- a = the result of applying the implicit argument to cap
  b <- rev  -- a = the result of applying the implicit argument to rev
  return (a, b) -- return = const

tupledM' :: [Char] -> ([Char], [Char])
tupledM' =
  cap >>= \a ->
  rev >>= \b ->
  return (a, b)


newtype Reado r a =
  Reado { getReado :: r -> a }


instance Functor (Reado r) where
  fmap f (Reado g) = Reado (\r -> f (g r)) -- f . g


ask :: Reado a a
ask = Reado id


-- pure :: a -> f a
-- pure :: a -> (r -> a)
--
--
-- (<*>) :: f (a -> b) -> f a -> f b
-- (<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)

instance Applicative (Reado r) where
  pure = Reado . const
  (<*>) (Reado rab) (Reado ra) = Reado (\r -> rab r (ra r))


instance Monad (Reado r) where
  -- (>>=) :: m a -> (a -> m b) -> m b
  -- (>>=) :: ((->) r a) -> (a -> ((->) r b)) -> ((->) r b)
  -- (>>=) :: (r -> a) -> (a -> r -> b) -> (r -> b)
  (>>=) :: Reado r a -> (a -> Reado r b) -> Reado r b
  (>>=) (Reado f) g = Reado (\r -> let (Reado h) = g (f r) in h r)
