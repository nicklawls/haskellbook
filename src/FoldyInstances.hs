module FoldyInstances where

import           Data.Monoid

data Constant a b =
  Constant a

instance Foldable (Constant a) where
  foldr _ b _ = b


data Two a b =
  Two a b

instance Foldable (Two a) where
  foldr f target (Two _ source) = f source target

data Three a b c =
  Three a b c

instance Foldable (Three a b) where
  foldr f target (Three _ _ source) = f source target


data Three' a b =
  Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' _ x y) = f x <> f y


data Four' a b =
  Four' a b b b

instance Foldable (Four' a) where
  foldMap toMonoid (Four' _ x y z) =
    foldMap toMonoid [x,y,z]


filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF p = foldMap (\a -> if pope a then pure a else mempty)



