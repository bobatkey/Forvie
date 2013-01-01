-- |
-- Module      :  Data.BooleanAlgebra
-- Copyright   :  Robert Atkey 2012
-- License     :  BSD3
--
-- Maintainer  :  bob.atkey@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--

module Data.BooleanAlgebra
    ( BooleanAlgebra (..)

    , Conjunctive (..)
    , and
    , all

    , Disjunctive (..)
    , or
    , any

    , prop_or_assoc
    , prop_or_commutative
    , prop_and_assoc
    , prop_and_commutative
    , prop_distrib1
    , prop_distrib2
    , prop_absorb1
    , prop_absorb2
    , prop_complement1
    , prop_complement2
    )
    where

import Prelude hiding (and, all, or, any)
import Data.Monoid (Monoid (..))
import Data.Foldable (Foldable, foldMap)

class BooleanAlgebra a where
    (.&.)      :: a -> a -> a
    (.|.)      :: a -> a -> a
    complement :: a -> a
    one        :: a
    zero       :: a

--------------------------------------------------------------------------------
instance BooleanAlgebra Bool where
    (.&.)      = (&&)
    (.|.)      = (||)
    complement = not
    one        = True
    zero       = False

--------------------------------------------------------------------------------
newtype Conjunctive a = Conjunctive { fromConjunctive :: a }

instance BooleanAlgebra a => Monoid (Conjunctive a) where
    mempty = Conjunctive one
    mappend (Conjunctive x) (Conjunctive y) = Conjunctive (x .&. y)

and :: (BooleanAlgebra a, Foldable t) => t a -> a
and = fromConjunctive . foldMap Conjunctive

all :: (BooleanAlgebra b, Foldable t) => (a -> b) -> t a -> b
all f = fromConjunctive . foldMap (Conjunctive . f)

--------------------------------------------------------------------------------
newtype Disjunctive a = Disjunctive { fromDisjunctive :: a }

instance BooleanAlgebra a => Monoid (Disjunctive a) where
    mempty = Disjunctive zero
    mappend (Disjunctive x) (Disjunctive y) = Disjunctive (x .|. y)

or :: (BooleanAlgebra a, Foldable t) => t a -> a
or = fromDisjunctive . foldMap Disjunctive

any :: (BooleanAlgebra b, Foldable t) => (a -> b) -> t a -> b
any f = fromDisjunctive . foldMap (Disjunctive . f)

--------------------------------------------------------------------------------

prop_or_assoc :: (BooleanAlgebra a, Eq a) => a -> a -> a -> Bool
prop_or_assoc a b c     = a .|. (b .|. c) == (a .|. b) .|. c

prop_or_commutative :: (BooleanAlgebra a, Eq a) => a -> a -> Bool
prop_or_commutative a b = a .|. b == b .|. a

prop_and_assoc :: (BooleanAlgebra a, Eq a) => a -> a -> a -> Bool
prop_and_assoc a b c     = a .&. (b .&. c) == (a .&. b) .&. c

prop_and_commutative :: (BooleanAlgebra a, Eq a) => a -> a -> Bool
prop_and_commutative a b = a .&. b == b .&. a

prop_distrib1 :: (BooleanAlgebra a, Eq a) => a -> a -> a -> Bool
prop_distrib1 a b c = a .|. (b .&. c) == (a .|. b) .&. (a .|. c)

prop_distrib2 :: (BooleanAlgebra a, Eq a) => a -> a -> a -> Bool
prop_distrib2 a b c = a .&. (b .|. c) == (a .&. b) .|. (a .&. c)

prop_absorb1 :: (BooleanAlgebra a, Eq a) => a -> a -> Bool
prop_absorb1 a b = a .|. (a .&. b) == a

prop_absorb2 :: (BooleanAlgebra a, Eq a) => a -> a -> Bool
prop_absorb2 a b = a .&. (a .|. b) == a

prop_complement1 :: (BooleanAlgebra a, Eq a) => a -> Bool
prop_complement1 a = a .&. complement a == zero

prop_complement2 :: (BooleanAlgebra a, Eq a) => a -> Bool
prop_complement2 a = a .|. complement a == one

