module Data.Knot where

import Data.Type.Show

data Knot f a = Knot (f (Knot f) a)

instance Show4 f => Show2 (Knot f) where
    show2 (Knot e) = "[" ++ show4 e ++ "]"

instance Show4 f => Show (Knot f a) where
    show (Knot e) = show4 e
