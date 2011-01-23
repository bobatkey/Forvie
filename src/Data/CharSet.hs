{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}

-- |
-- Module      :  Data.CharSet
-- Copyright   :  Robert Atkey 2010
-- License     :  BSD3
--
-- Maintainer  :  Robert.Atkey@cis.strath.ac.uk
-- Stability   :  experimental
-- Portability :  unknown
--
-- Representation of unicode character sets, based on cset.ml from
-- Alain Frisch's ulex for ocaml. One difference is that because we
-- are using 'Char's to represent characters, the implementation has
-- to be more careful about going off the end of the range of
-- expressible characters.

module Data.CharSet
    (
      -- * Character set type
      CSet
    , anyChar
    , singleton 
    , interval
      
      -- * Useful Character Sets
    , xmlNameStartChar
    , xmlNameChar
    , mathematicalOperators
    , nameStartChar
    , nameChar
    , digit
    , space)
    where

import Prelude hiding (null)
import Data.List (intercalate)
import Data.String
import Data.BooleanAlgebra
import Data.RangeSet

type CSet = Set Char

instance IsString CSet where
    fromString = foldl (.|.) zero . map singleton

anyChar :: CSet
anyChar = one

{------------------------------------------------------------------------------}
-- Some character sets
-- FIXME: split this out?
xmlNameStartChar = singleton ':'
                   .|.
                   interval 'A' 'Z'
                   .|.
                   singleton '_'
                   .|.
                   interval 'a' 'z'
                   .|.
                   interval '\xc0' '\xd6'
                   .|.
                   interval '\xd8' '\xf6'
                   .|.
                   interval '\x370' '\x37d'
                   .|.
                   interval '\x37f' '\x1fff'
                   .|.
                   interval '\x200c' '\x200d'
                   .|.
                   interval '\x2070' '\x218f'
                   .|.
                   interval '\x2c00' '\x2fef'
                   .|.
                   interval '\x3001' '\xd7ff'
                   .|.
                   interval '\xf900' '\xFDCF'
                   .|.
                   interval '\xfdf0' '\xfffd'
                   .|.
                   interval '\x10000' '\xeffff'

xmlNameChar  = xmlNameStartChar
               .|.
               singleton '-'
               .|.
--               singleton '.'
--               .|.
               interval '0' '9'
               .|.
               singleton '\xb7'
               .|.
               interval '\x0300' '\x036f'
               .|.
               interval '\x203f' '\x2040'

mathematicalOperators = interval '\x2200' '\x22ff'

nameStartChar = xmlNameStartChar .|. mathematicalOperators
nameChar = xmlNameChar .|. mathematicalOperators .|. singleton '\''

digit = interval '0' '9'

space :: CSet
space = " \n\t"
