{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies, FlexibleContexts, FlexibleInstances #-}

module Main
    ( main )
    where

import           Prelude hiding (null)
import           Data.Maybe (fromJust)
import           Control.Applicative
import           System.Random
import           Test.Framework (Test, defaultMain, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.QuickCheck hiding (ranges, Result)

import           Data.RangeSet
import           Data.FiniteStateMachine
import           Data.FiniteStateMachine.Deterministic
import           Data.FiniteStateMachine.RegexpDerivatives

import           Layout
import           LayoutHUnit

--------------------------------------------------------------------------------
-- from http://www.cubiclemuses.com/cm/articles/2011/07/14/quickchecking-type-class-laws/
asFunctionOf :: (a -> b) -> a -> (a -> b)
asFunctionOf f _ = f

--------------------------------------------------------------------------------
type TypeIndexedTest a = a -> Test

typeIndexedTest :: forall a. TypeIndexedTest a -> Test
typeIndexedTest t = t (undefined :: a)

--------------------------------------------------------------------------------
test_BooleanAlgebra :: forall a.
                       (BooleanAlgebra a, Eq a, Arbitrary a, Show a) =>
                       TypeIndexedTest a
test_BooleanAlgebra t
    = testGroup "Boolean algebra laws"
      [ testProperty "or is associative" (prop_or_assoc `asFunctionOf` t)
      , testProperty "or is commutative" (prop_or_commutative `asFunctionOf` t)
      , testProperty "and is associative" (prop_and_assoc `asFunctionOf` t)
      , testProperty "and is commutative" (prop_and_commutative `asFunctionOf` t)
      , testProperty "or/and distribute on the left" (prop_distrib1 `asFunctionOf` t)
      , testProperty "or/and distribute on the right" (prop_distrib2 `asFunctionOf` t)
      , testProperty "absorbtion law 1" (prop_absorb1 `asFunctionOf` t)
      , testProperty "absorbtion law 2" (prop_absorb2 `asFunctionOf` t)
      , testProperty "a and not a is zero" (prop_complement1 `asFunctionOf` t)
      , testProperty "a or not a is one" (prop_complement2 `asFunctionOf` t)
      ]

--------------------------------------------------------------------------------
instance (Ord a, Bounded a, Enum a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = foldl (.|.) zero <$> (map (uncurry interval) <$> arbitrary)

prop_interval :: (Random a, Ord a) => a -> a -> Property
prop_interval a b = do
  c <- choose (a,b)
  property (c `member` (interval a b))

prop_ranges :: (Bounded a, Enum a, Ord a) =>
               Set a ->
               Bool
prop_ranges a =
    (foldl (.|.) zero . map (uncurry interval) . ranges) a == a

prop_singleton :: Ord a => a -> Bool
prop_singleton a = a `member` (singleton a)

prop_representative :: Ord a => Set a -> Property
prop_representative s = 
    not (null s) ==>
    (fromJust $ getRepresentative s) `member` s

--------------------------------------------------------------------------------
{-
compareFSAs :: ( FiniteStateAcceptor a
               , FiniteStateAcceptor b
               , Alphabet a ~ Alphabet b
               , Result a ~ Result b
               , Arbitrary (Alphabet a)
               , Show (Alphabet a)
               , Eq (Result a)) =>
               a ->
               b ->
               Property
compareFSAs a b = property $ \input -> runFiniteStateAcceptor a input == runFiniteStateAcceptor b input
-}

-- based on:
-- https://github.com/sebfisch/haskell-regexp/blob/master/src/quickcheck.lhs
instance Arbitrary (Regexp Char) where
    arbitrary = sized regexp

regexp :: Int -> Gen (Regexp Char)
regexp 0 = frequency [ (1, return one)
                     , (8, tok <$> (singleton <$> elements "abcdef"))
                     ]
regexp n = frequency [ (3, regexp 0)
                     , (1, (.|.) <$> subexp <*> subexp)
                     , (2, (.>>.) <$> subexp <*> subexp)
                     , (1, zeroOrMore <$> regexp (n-1))
                     ]
    where subexp = regexp (n `div` 2)

newtype RegexpInput = RegexpInput String
    deriving Show

instance Arbitrary RegexpInput where
    arbitrary = RegexpInput <$> listOf1 (elements "abcdef")

-- FIXME: problem with this is that it spends most of its not matching
-- anything, so we only test part of the specification.
prop_regexp :: Regexp Char -> RegexpInput -> Bool
prop_regexp (r :: Regexp Char) (RegexpInput i) =
    runFSM r i == runFSM (makeDFA r) i

--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain
    [ testGroup "Data.RangeSet"
      [ typeIndexedTest (test_BooleanAlgebra :: TypeIndexedTest (Set Char))
      , testProperty "interval"       (prop_interval `asFunctionOf` (undefined :: Char))
      , testProperty "ranges"         (prop_ranges `asFunctionOf` (undefined :: Set Char))
      , testProperty "singleton"      (prop_singleton `asFunctionOf` (undefined :: Char))
      , testProperty "representative" (prop_representative `asFunctionOf` (undefined :: Set Char))
      ]
    , testGroup "Data.Regexp"
      [ testProperty "regexp" prop_regexp
      ]
    , testGroup "Language.Forvie.Layout"
      [ testProperty "layout" prop_layout
      , testCase "layout-hunit" layoutTestCases
      ]
    ]
