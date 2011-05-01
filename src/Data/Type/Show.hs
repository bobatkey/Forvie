{-# LANGUAGE KindSignatures #-}

module Data.Type.Show where

class Show2 f where
    show2 :: f a -> String

class Show3 f where
    show3 :: f a b -> String

class Show4 (f :: (* -> *) -> * -> *) where
    show4 :: Show2 v => f v a -> String
