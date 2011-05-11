{-# LANGUAGE TypeFamilies, FlexibleInstances #-}

-- |
-- Module          :  Data.Regexp
-- Copyright       :  Robert Atkey 2011
-- License         :  BSD3
--
-- Maintainer      :  Robert.Atkey@cis.strath.ac.uk
-- Stability       :  experimental
-- Portability     :  unknown
--
-- Regular expressions.

module Data.Regexp
    ( Regexp ()
    , (.>>.)
    , star
    , zeroOrMore
    , oneOrMore
    , tok
    , subst
    , module Data.BooleanAlgebra
    )
    where

import qualified Data.Set as S
import           Data.String
import           Data.RangeSet
import qualified Data.DFA as DFA
import           Data.BooleanAlgebra

{------------------------------------------------------------------------------}
classesN :: (Enum a, Bounded a, Ord a) => Regexp a -> Partition a
classesN (NSeq ns) = classesNs ns
    where
      classesNs []          = fromSet one
      classesNs (n:ns) 
          | matchesEmptyN n = classesN n `andClasses` classesNs ns
          | otherwise       = classesN n
classesN (NAlt ns) = foldr andClasses (fromSet one) $ map classesN $ S.elems ns
classesN (NTok c)  = fromSet c
classesN (NStar n) = classesN n
classesN (NAnd ns) = foldr andClasses (fromSet one) $ map classesN $ S.elems ns
classesN (NNot n)  = classesN n

{------------------------------------------------------------------------------}
-- Class instance

boolToMaybe True  = Just ()
boolToMaybe False = Nothing

instance (Ord a, Enum a, Bounded a) => DFA.FiniteStateAcceptor (Regexp a) where
    type DFA.Alphabet (Regexp a) = a
    type DFA.Result   (Regexp a) = ()
    advance c        = diffN c
    isErrorState     = matchesNothingN
    isAcceptingState = boolToMaybe . matchesEmptyN
    classes          = classesN

{------------------------------------------------------------------------------}
matchesNothingN :: Regexp a -> Bool
matchesNothingN (NSeq ns) = or $ map matchesNothingN ns
matchesNothingN (NAlt ns) = and $ map matchesNothingN $ S.elems ns
matchesNothingN (NTok c)  = Data.RangeSet.null c
matchesNothingN (NAnd ns) = and $ map matchesNothingN $ S.elems ns
matchesNothingN (NNot n)  = not $ matchesNothingN n
matchesNothingN (NStar _) = False

matchesEmptyN :: Regexp a -> Bool
matchesEmptyN (NSeq ns) = and $ map matchesEmptyN ns
matchesEmptyN (NAlt ns) = or $ map matchesEmptyN $ S.elems ns
matchesEmptyN (NTok c)  = False
matchesEmptyN (NStar _) = True
matchesEmptyN (NNot n)  = not $ matchesEmptyN n
matchesEmptyN (NAnd ns) = and $ map matchesEmptyN $ S.elems ns

diffN :: Ord a => a -> Regexp a -> Regexp a
diffN c (NSeq ns)  = diffNs ns
    where
      diffNs []     = nZero
      diffNs (n:ns) 
          | matchesEmptyN n = (diffN c n `nSeq` NSeq ns) `nAlt` diffNs ns
          | otherwise       = diffN c n `nSeq` NSeq ns
diffN c (NAlt ns)  = foldr nAlt nZero $ map (diffN c) $ S.elems ns
diffN c (NTok cl)
    | c `memberOf` cl = nEps
    | otherwise       = nZero
diffN c (NStar ns) = diffN c ns `nSeq` NStar ns
diffN c (NAnd ns)  = foldr nAnd nTop $ map (diffN c) $ S.elems ns
diffN c (NNot n)   = NNot (diffN c n)

{------------------------------------------------------------------------------}
subst :: Ord b => (Set a -> Regexp b) -> Regexp a -> Regexp b
subst f (NSeq l)  = foldr nSeq nEps $ map (subst f) l
subst f (NAlt l)  = foldr nAlt nZero $ map (subst f) $ S.elems l
subst f (NTok s)  = f s
subst f (NAnd l)  = foldr nAnd nTop $ map (subst f) $ S.elems l
subst f (NNot r)  = NNot (subst f r)
subst f (NStar r) = nStar (subst f r)

{------------------------------------------------------------------------------}
data Regexp a
    = NSeq  [Regexp a]
    | NAlt  (S.Set (Regexp a))
    | NTok  (Set a)
    | NAnd  (S.Set (Regexp a))
    | NNot  (Regexp a)
    | NStar (Regexp a)
      deriving (Eq, Ord, Show)

instance IsString (Regexp Char) where
    fromString s = NSeq $ map (NTok . singleton) s

instance Ord a => BooleanAlgebra (Regexp a) where
    (.&.)      = nAnd
    (.|.)      = nAlt
    complement = NNot
    one        = nTop
    zero       = nZero

(.>>.) :: Regexp a -> Regexp a -> Regexp a
(.>>.) = nSeq

star :: Regexp a -> Regexp a
star n = NStar n

zeroOrMore :: Regexp a -> Regexp a
zeroOrMore = star

oneOrMore :: Regexp a -> Regexp a
oneOrMore n = n .>>. star n

tok :: Set a -> Regexp a
tok cs = NTok cs

nEps  = NSeq []
nZero = NAlt S.empty
nTop  = NNot nZero

isBottom (NAlt s) = S.null s
isBottom _        = False

isTop (NNot x) = isBottom x
isTop _        = False

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

nStar (NStar r) = NStar r
nStar (NSeq []) = NSeq []
nStar x | isBottom x = NSeq []
        | otherwise  = NStar x
