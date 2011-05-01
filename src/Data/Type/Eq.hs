{-# LANGUAGE TypeOperators, GADTs #-}

module Data.Type.Eq where

import Data.Type.Equality

class Eq2 f where
    (===) :: f a -> f b -> Maybe (a :=: b)

class Eq3 f where
    (====) :: f a b -> f c d -> Maybe (a :=: c, b :=: d)

compareHelper :: Eq a => Maybe (a :=: b, c :=: d) -> a -> b -> Maybe (c :=: d)
compareHelper Nothing             a b  = Nothing
compareHelper (Just (Refl, Refl)) a a' = if a == a' then Just Refl else Nothing
