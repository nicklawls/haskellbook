module Foldy where

import           Data.Monoid

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum

product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product

elem :: (Foldable t, Ord a) => a -> t a -> Bool
elem a = any ((==) a)

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr (\next acc -> case acc of
                              Nothing -> Just next
                              Just foo -> Just (min next foo)) Nothing

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr (\next -> maybe (Just next) (\a -> Just (max next a))) Nothing


null :: (Foldable t) => t a -> Bool
null = foldr (const (const False)) True

length :: (Foldable t) => t a -> Int
length = foldr (const ((+) 1)) 0

toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

foldMappy :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMappy f = foldr (\next acc -> f next <> acc) mempty
