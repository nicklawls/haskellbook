{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}

module Trav where

import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

newtype Identity a = Identity a
  deriving (Eq, Ord, Show, Functor)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Foldable Identity where
  foldMap f (Identity a ) = f a

instance Traversable Identity where
  traverse aToFb (Identity a) = Identity <$> (aToFb a)

newtype Constant a b =
  Constant { getConstant :: a }
    deriving (Eq, Ord, Show, Functor)

instance Foldable (Constant a) where
  foldMap _ c = mempty

instance Traversable (Constant a) where
  traverse a1ToFb (Constant a) = pure (Constant a)

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance (Eq a, Eq b)=> EqProp (Constant a b) where
  (=-=) = eq

data Optional a =
    Nada
  | Yep a
  deriving (Eq, Ord, Show, Functor)

instance Foldable Optional where
  foldMap f op =
    case op of
      Nada -> mempty
      Yep a -> f a

instance Traversable Optional where
  traverse aToFb op =
    case op of
      Nada -> pure Nada
      Yep a -> Yep <$> aToFb a

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = oneof [pure Nada, Yep <$> arbitrary]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

data List a = Nil | Cons a (List a)
  deriving (Eq, Ord, Show, Functor)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = f x <> (foldMap f xs)

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> (traverse f xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    oneof [ pure Nil, Cons <$> arbitrary <*> arbitrary ]

instance Eq a => EqProp (List a) where
  (=-=) = eq

data Three a b c =
  Three a b c
    deriving (Eq, Show, Functor)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

data Three' a b =
  Three' a b b
    deriving (Eq, Show, Functor)

instance Foldable (Three' a) where
  foldMap f (Three' _ b1 b2) = f b1 <> f b2

instance Traversable (Three' a) where
  traverse f (Three' a b1 b2) = Three' a <$> f b1 <*> f b2

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq


data S n a = S (n a) a
  deriving (Eq, Show)

instance Functor n => Functor (S n) where
  -- to transform the a inside the n a to a be, we need Functor n
  fmap f (S na a) = S (f <$> na) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S na a) = foldMap f na <> f a

instance Traversable n => Traversable (S n) where
  -- now, to flip the containers around, we need Traversable n
  traverse f (S na a) = S <$> traverse f na <*> f a

instance (Arbitrary a, Arbitrary (n a)) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Eq a, Eq (n a)) => EqProp (S n a) where
  (=-=) = eq

data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show, Functor)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node l a r) = (foldMap f l) <> (f a) <> (foldMap f r)

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node l a r) =
    Node <$> traverse f l <*> f a <*> traverse f r

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary =
    oneof
      [ pure Empty
      , Leaf <$> arbitrary
      , Node <$> arbitrary <*> arbitrary <*> arbitrary
      ]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch (traversable (undefined :: Identity (Int, Int, String)))
  quickBatch (traversable (undefined :: Constant String (Int, Int, [Bool])))
  quickBatch (traversable (undefined :: Optional (Int, Int, [String])))
  quickBatch (traversable (undefined :: List (Int, Int, Maybe String)))
  quickBatch (traversable (undefined :: Three Bool String (Int, Int, Maybe String)))
  quickBatch (traversable (undefined :: Three' Float (Int, Int, Maybe String)))
  quickBatch (traversable (undefined :: S Maybe (Int, Int, Maybe String)))
  quickBatch (traversable (undefined :: Tree (Int, Int, [String])))
