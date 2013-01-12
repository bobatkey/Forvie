{-# LANGUAGE TypeFamilies, FlexibleInstances #-}

-- |
-- Module          :  Data.Regexp
-- Copyright       :  (C) Robert Atkey 2013
-- License         :  BSD3
--
-- Maintainer      :  bob.atkey@gmail.com
-- Stability       :  experimental
-- Portability     :  unknown
--
-- Regular expressions over an arbitrary bounded alphabet.

module Data.Regexp
    ( Regexp ()
    , (.>>.)
    , star
    , zeroOrMore
    , oneOrMore
    , tok
    , module Data.BooleanAlgebra
    , module Data.Monoid
    )
    where

import           Prelude hiding (all, any)
import qualified Data.Set as S
import           Data.Monoid
import           Data.BooleanAlgebra
import           Data.Foldable hiding (foldr, all, any)
import           Data.String (IsString (..))
import           Data.RangeSet
import           Control.FiniteStateMachine
import           Control.Monad (guard)

-- | Regular expressions over an arbitrary alphabet.
data Regexp alphabet
    = NSeq  [Regexp alphabet]
    | NAlt  (S.Set (Regexp alphabet))
    | NTok  (Set alphabet)
    | NAnd  (S.Set (Regexp alphabet))
    | NNot  (Regexp alphabet)
    | NStar (Regexp alphabet)
    deriving (Eq, Ord, Show)

{------------------------------------------------------------------------------}
-- | Match a string literal exactly.
instance IsString (Regexp Char) where
    fromString s = NSeq $ map (NTok . singleton) s

-- | Regular expressions form a boolean algebra.
instance Ord alphabet => BooleanAlgebra (Regexp alphabet) where
    (.&.)      = nAnd
    (.|.)      = nAlt
    complement = NNot
    one        = nTop
    zero       = nZero

-- | Sequencing of regular expressions.
instance Monoid (Regexp alphabet) where
    mempty  = NSeq []
    mappend = (.>>.)
    mconcat = NSeq

{------------------------------------------------------------------------------}
-- | Regular expressions are directly finite state machines via
-- Brzozowski derivatives.
instance (Ord a, Enum a, Bounded a) => FiniteStateMachine (Regexp a) where
    type State    (Regexp a) = Regexp a
    type Alphabet (Regexp a) = a
    type Result   (Regexp a) = ()
    initState r        = r
    advance _ c        = diffN c
    isAcceptingState _ = guard . matchesEmptyN
    classes _          = classesN

{------------------------------------------------------------------------------}
matchesEmptyN :: Regexp a -> Bool
matchesEmptyN (NSeq ns) = all matchesEmptyN ns
matchesEmptyN (NAlt ns) = any matchesEmptyN ns
matchesEmptyN (NTok c)  = False
matchesEmptyN (NStar _) = True
matchesEmptyN (NNot n)  = not (matchesEmptyN n)
matchesEmptyN (NAnd ns) = all matchesEmptyN ns

classesN :: (Enum a, Bounded a, Ord a) => Regexp a -> Partition a
classesN (NSeq ns) = classesNs ns
    where
      classesNs []          = fromSet one
      classesNs (n:ns)
          | matchesEmptyN n = classesN n `mappend` classesNs ns
          | otherwise       = classesN n
classesN (NAlt ns) = foldMap classesN ns
classesN (NTok c)  = fromSet c
classesN (NStar n) = classesN n
classesN (NAnd ns) = foldMap classesN ns
classesN (NNot n)  = classesN n

diffN :: Ord a => a -> Regexp a -> Regexp a
diffN c (NSeq ns)  = diffNs ns
    where
      diffNs []     = nZero
      diffNs (n:ns)
          | matchesEmptyN n = (diffN c n `nSeq` NSeq ns) `nAlt` diffNs ns
          | otherwise       = diffN c n `nSeq` NSeq ns
diffN c (NAlt ns)  = any (diffN c) ns
diffN c (NTok cl)
    | c `memberOf` cl = nEps
    | otherwise       = nZero
diffN c (NStar ns) = diffN c ns `nSeq` NStar ns
diffN c (NAnd ns)  = all (diffN c) ns
diffN c (NNot n)   = NNot (diffN c n)


{------------------------------------------------------------------------------}
-- | Sequential sequencing of regular expressions.
(.>>.) :: Regexp a -> Regexp a -> Regexp a
(.>>.) = nSeq

-- | @star re@ matches zero or more repetitions of @re@. Synonym of
-- 'zeroOrMore'.
star :: Regexp a -> Regexp a
star n = NStar n

-- | @zeroOrMore re@ matches zero or more repetitions of @re@. Synonym
-- of 'star'.
zeroOrMore :: Regexp a -> Regexp a
zeroOrMore = star

-- | @oneOrMore re@ matches one or more repetitions of
-- @re@. Equivalent to @re .>>. star re@.
oneOrMore :: Regexp a -> Regexp a
oneOrMore n = n .>>. star n

-- | A regular expression that accepts a single token from the given
-- set of tokens.
tok :: Set a -> Regexp a
tok cs = NTok cs

nEps :: Regexp a
nEps  = NSeq []

nZero :: Regexp a
nZero = NAlt S.empty

nTop :: Regexp a
nTop  = NNot nZero

isBottom :: Regexp a -> Bool
isBottom (NAlt s) = S.null s
isBottom _        = False

isTop :: Regexp a -> Bool
isTop (NNot x) = isBottom x
isTop _        = False

nSeq :: Regexp a -> Regexp a -> Regexp a
nSeq (NSeq x) (NSeq y) = NSeq (x ++ y)
nSeq x        (NSeq y)
    | isBottom x       = nZero
    | otherwise        = NSeq ([x] ++ y)
nSeq (NSeq x) y
    | isBottom y       = nZero
    | otherwise        = NSeq (x ++ [y])
nSeq x          y
    | isBottom x       = nZero
    | isBottom y       = nZero
    | otherwise        = NSeq [x,y]

nAlt :: Ord a => Regexp a -> Regexp a -> Regexp a
nAlt (NAlt x) (NAlt y) = NAlt (S.union x y)
nAlt x        (NAlt y) 
    | isTop x          = NNot nZero
    | otherwise        = NAlt (S.insert x y)
nAlt (NAlt x) y        
    | isTop y          = NNot nZero
    | otherwise        = NAlt (S.insert y x)
nAlt x        y        
    | isTop x          = NNot nZero
    | isTop y          = NNot nZero
    | otherwise        = NAlt (S.fromList [x,y])

-- the cancellation rules are essential for termination of the DFA
-- construction algorithm (need the ones for 'and' as well as
-- 'or'. This is not mentioned in the Owens et al paper, but they
-- implement it anyway).
nAnd :: Ord a => Regexp a -> Regexp a -> Regexp a
nAnd (NAnd x) (NAnd y) = NAnd (S.union x y)
nAnd x        (NAnd y) 
    | isTop x          = NAnd y
    | isBottom x       = nZero
    | otherwise        = NAnd (S.insert x y)
nAnd (NAnd x) y
    | isTop y          = NAnd x
    | isBottom y       = nZero
    | otherwise        = NAnd (S.insert y x)
nAnd x        y
    | isTop x          = y
    | isTop y          = x
    | isBottom x       = nZero
    | isBottom y       = nZero
    | otherwise        = NAnd (S.fromList [x,y])

-- I wonder why this isn't used?
-- nStar (NStar r) = NStar r
-- nStar (NSeq []) = NSeq []
-- nStar x | isBottom x = NSeq []
--         | otherwise  = NStar x
