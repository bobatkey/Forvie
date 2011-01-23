{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

-- |
-- Module      :  Data.RangeSet
-- Copyright   :  Robert Atkey 2010
-- License     :  BSD3
--
-- Maintainer  :  Robert.Atkey@cis.strath.ac.uk
-- Stability   :  experimental
-- Portability :  unknown
--
-- Representation of sets of elements of ('Bounded', 'Ord', 'Enum')
-- instances. Such types have a total ordering with minimum and
-- maximum elements. Sets are represented as ranges of elements.
--
-- The implementation is based on the implementation of sets of
-- Unicode code points in cset.ml from Alain Frisch's ulex for
-- OCaml. One difference is that because this implementation uses
-- instances of 'Bounded'to represent characters, it has to be more
-- careful about going off the end of the range.

module Data.RangeSet
    (
    -- * Range Set type
      Set ()
      
    -- * Construction 
    , singleton
    , interval
      
    -- * Queries
    , memberOf
    , getRepresentative
    , null
    , ranges
    
    
    -- * Partitions
    , Partition
    , fromSet
    , andClasses
      
    -- * Maps
    , TotalMap
    , makeTotalMap
    , lookup
    )
    where

import           Prelude hiding (null, lookup)
import qualified Data.Set as S
import           Data.List (intercalate)
import           Data.Maybe (fromJust)
import           Data.String (IsString)
import           Data.BooleanAlgebra
import           Data.Functor
import           Test.QuickCheck hiding (ranges)
import           Data.Array

{- Representation of character sets, copied from cset.ml from Alain
   Frisch's ulex for ocaml. Only difference is that because we are
   using 'Char's to represent characters, the implementation has to be
   more careful about going off the end of the range of expressible
   characters. -}

{- Character sets are represented as increasing, non-overlapping,
   non-empty lists of intervals -}

-- TODO: coalescing of ranges that abutt. Seems to do this already?

-- | A set of unicode codepoints. Note that this type is an instance
-- of 'IsString', so character sets can be specified as literal
-- strings.
newtype Set a = Set { unSet :: [(a,a)] }
    deriving (Eq,Ord)

isNormalForm :: Ord a => Set a -> Bool
isNormalForm (Set l) = check1 l
    where
      check1 []        = True
      check1 ((a,b):l) = a <= b && check b l
          
      check x []        = True
      check x ((a,b):l) = x < a && a <= b && check b l

-- FIXME: this sucks, would be (slightly) better to generate a list of
-- ranges and then union them
instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = (Set <$> arbitrary) `suchThat` isNormalForm

instance (Enum a, Ord a, Bounded a) => BooleanAlgebra (Set a) where
    (.&.)      = intersect
    (.|.)      = union
    complement = Data.RangeSet.complement
    one        = everything
    zero       = empty

-- | Returns the list of ranges of elements represented by the given
-- 'Set'. The end points of each range are inclusive, and the ranges
-- are in increasing order.
ranges :: Set a -> [(a,a)]
ranges (Set l) = l

instance (Eq a, Show a) => Show (Set a) where
    show (Set l) = "[" ++ intercalate "," (map showRange l) ++ "]"
        where showRange (l,h)
                  | l == h    = show l
                  | otherwise = show l ++ "-" ++ show h

-- | The empty set
empty :: Set a
empty = Set []

-- FIXME: could shortcut search if i > c.
-- | Test to determine whether the given 'a' is in the 'Set a'.
memberOf :: Ord a => a -> Set a -> Bool
memberOf c (Set l) = aux c l
    where
      aux c []        = False
      aux c ((i,j):l) = (i <= c && c <= j) || aux c l

-- | Returns an arbitrary element in the given set. Returns
-- 'Nothing' if the set is empty.
getRepresentative :: Set a -> Maybe a
getRepresentative (Set [])        = Nothing
getRepresentative (Set ((i,_):_)) = Just i

-- | Set consisting of a single unicode code point.
singleton :: a -> Set a
singleton c = Set [(c,c)]

-- | Test to determine whether the given set is empty.
null :: Set a -> Bool
null (Set []) = True
null (Set _)  = False

-- | Sets containing all the characters between the two given
-- endpoints, inclusive of these points. The arguments need not be in
-- any particular order.
interval :: Ord a => a -> a -> Set a
interval i j = Set $ if i <= j then [(i,j)] else [(j,i)]

-- | The set containing all of the elements of 'a'
everything :: (Ord a, Bounded a) => Set a
everything = interval minBound maxBound

-- | Computes the union of two character sets, i.e. the set with all
-- the characters contained in the two arguments.
union :: (Enum a, Bounded a, Ord a) => Set a -> Set a -> Set a
union (Set c1) (Set c2) = Set $ aux c1 c2
    where
      aux [] c2 = c2
      aux c1 [] = c1
      aux c1@((i1,j1):r1) c2@((i2,j2):r2) =
          if i1 <= i2 then
              if j1 /= maxBound && succ j1 < i2 then (i1,j1):aux r1 c2
              else if j1 < j2 then aux r1 ((i1,j2):r2)
                   else aux c1 r2
          else aux c2 c1

-- | Computes the complement of the given set, i.e. the set of all
-- elements not in the given set.
complement :: (Enum a, Bounded a, Ord a) => Set a -> Set a
complement = Set . aux minBound . unSet
    where
      aux start []      
          | start <= maxBound = [(start,maxBound)]
          | otherwise         = []
      aux start ((i,j):l)
          | start == i        = if j == maxBound then [] else aux (succ j) l
          | otherwise         = (start,pred i):if j == maxBound then [] else aux (succ j) l

-- | Computes the intersection of two sets. Returns the set containing
-- all the elements found in both of the given sets.
intersect :: (Enum a, Bounded a, Ord a) => Set a -> Set a -> Set a
intersect c1 c2 = Data.RangeSet.complement (Data.RangeSet.complement c1 `union` Data.RangeSet.complement c2)

{------------------------------------------------------------------------------}
-- FIXME: Need to do something with this
prop_interval a b c = a <= c && c <= b ==> c `memberOf` (interval a b)

--------------------------------------------------------------------------------
newtype Partition a = Partition (S.Set (Set a))

fromSet :: (Enum a, Bounded a, Ord a) => Set a -> Partition a
fromSet s 
    | s == everything = Partition (S.fromList [ s ])
    | s == empty      = Partition (S.fromList [ s ])
    | otherwise       = Partition (S.fromList [ s, Data.RangeSet.complement s ])

andClasses :: (Enum a, Bounded a, Ord a) =>
              Partition a -> Partition a -> Partition a
andClasses (Partition x) (Partition y) =
    Partition $ S.fromList [ a `intersect` b | a <- S.elems x, b <- S.elems y ]

--------------------------------------------------------------------------------
-- FIXME: need a better representation than this
-- Should be a balanced tree
newtype TotalMap a b = TotalMap [(Set a,b)]

makeTotalMap :: Partition a -> (a -> b) -> TotalMap a b
makeTotalMap (Partition sets) f =
    TotalMap [ (set, f (fromJust $ getRepresentative set)) | set <- S.elems sets ]
                       
lookup :: Ord a => TotalMap a b -> a -> b
lookup (TotalMap mappings) a = go mappings
    where 
      go []        = error "internal error: not a total map"
      go ((s,b):m) = if a `memberOf` s then b else go m


      {-
data Partition a b
    = Partition { mapping :: Array Int b
                , classes :: [(a,a,Int)]
                }
      deriving Show

everywhere :: Bounded a => b -> Partition a b
everywhere b
    = Partition { mapping = array (0,0) [(0,b)]
                , classes = [(minBound, maxBound, 0)]
                }

ifThenElse :: (Enum a, Ord a, Bounded a) => Set a -> b -> b -> Partition a b
ifThenElse (Set ranges) t e =
    Partition { mapping = array (0,1) [(0,t), (1,e)]
              , classes = build minBound ranges
              }
    where
      build x []        = if x == maxBound then [] else [(x,maxBound,1)]
      build x ((i,j):r) = if x < i then (x,pred i,1):tail else tail
          where tail = (i,j,0):if j == maxBound then [] else build (succ j) r

intersectPartitions :: Partition a b -> Partition a b -> Partition a b
intersectPartitions 
-}