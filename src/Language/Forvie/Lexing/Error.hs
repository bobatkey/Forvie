{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module         : Language.Forvie.Lexing.Error
-- Copyright      : Robert Atkey 2011
-- License        : BSD3
--
-- Maintainer     : Robert.Atkey@cis.strath.ac.uk
-- Stability      : experimental
-- Portability    : unknown
--
-- Representation of Lexing Errors.

module Language.Forvie.Lexing.Error
    ( LexingError (..)
    )
    where

import Text.Position (Position)

-- | A class capturing the kinds of error that can occur during
-- lexing. A simple 'String' instance is provided that gives a human
-- readable error message.
class LexingError e where
    -- | There has been a lexing error, either at the end of the input
    -- (when the argument is `Nothing`), or on a specific character
    -- (when the argument is `Just (c,p)`).
    lexingErr :: Maybe (Char, Position) -> e

instance LexingError String where
    lexingErr _ = "Lexing error" -- FIXME
