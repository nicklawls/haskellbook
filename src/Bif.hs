module Bif
    (
    ) where


import Data.Bifunctor

data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)

data Const a b = Const a

instance Bifunctor Const where
  bimap f _ (Const a) = Const (f a)

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)

data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a


data FarQuad a b c d = Far a b c d

instance Bifunctor (FarQuad a b) where
  bimap f g (Far a b c d) = Far a b (f c) (g d)

data Sum a b = L a | R b


instance Bifunctor Sum where
  bimap f _ (L a) = L (f a)
  bimap _ g (R b) = R (g b)