{-# LANGUAGE GADTs #-}

module Data.TypedSet
    ( Set ()
    , empty
    , insert
    , member
    )
    where

import Data.TypedMap (Eq1 (..), Compare1 (..), Ord1 (..))
import qualified Data.Set as S

type Set f = S.Set (Datum f)

data Datum f where
    Datum :: f a -> Datum f

instance Eq1 x => Eq (Datum x) where
    Datum x1 == Datum x2 =
        case x1 === x2 of Nothing -> False; Just _ -> True

instance Ord1 x => Ord (Datum x) where
    compare (Datum x1) (Datum x2) =
        case compare1 x1 x2 of LT1 -> LT; EQ1 -> EQ; GT1 -> GT

empty :: Set f
empty = S.empty

insert :: Ord1 f => f a -> Set f -> Set f
insert x = S.insert (Datum x)

member :: Ord1 f => f a -> Set f -> Bool
member x = S.member (Datum x)