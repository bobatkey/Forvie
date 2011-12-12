{-# LANGUAGE GADTs #-}

module Data.TypedMap where

-- Move the stuff below elsewhere
class Show1 f where
    show1 :: f a -> String

class Show2 f where
    show2 :: Show1 v => f v a -> String

data Equal a b where
    Refl :: Equal a a

data Compare1 a b where
    LT1 :: Compare1 a b
    EQ1 :: Compare1 a a
    GT1 :: Compare1 a b

class Eq1 f where
    (===) :: f a -> f b -> Maybe (Equal a b)

class Eq1 f => Ord1 f where
    compare1 :: f a -> f b -> Compare1 a b

data P a f b = P a (f b)

instance (Eq a, Eq1 f) => Eq1 (P a f) where
    P a1 f1 === P a2 f2
        | a1 /= a2  = Nothing
        | otherwise = f1 === f2

-- Now write some kind of balanced binary tree that uses Ord1

-- In the meantime, just use association lists. These support multiple
-- associations of the same key to many values.
data Datum k v where
    Datum :: k a -> v a -> Datum k v

newtype Map k v = Map { assocs :: [Datum k v] }

empty :: Map k v
empty = Map []

insert :: k a -> v a -> Map k v -> Map k v
insert key value = Map . (Datum key value:) . assocs

lookup :: Eq1 k => k a -> Map k v -> Maybe (v a)
lookup key = findKey . assocs
    where findKey [] = Nothing
          findKey (Datum key' value:rest) =
              case key === key' of
                Nothing   -> findKey rest
                Just Refl -> Just value
