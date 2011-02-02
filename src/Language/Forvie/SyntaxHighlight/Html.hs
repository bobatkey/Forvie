{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module         :  Language.Forvie.SyntaxHighlight.Html
-- Copyright      :  Robert Atkey 2011
-- License        :  BSD3
--
-- Maintainer     :  Robert.Atkey@cis.strath.ac.uk
-- Stability      :  experimental
-- Portability    :  unknown
--
-- Functions for converting 'Lexeme's to HTML, classified according to
-- the 'SyntaxHighlight' class.

module Language.Forvie.SyntaxHighlight.Html
    ( generateHtml )
    where

import           Data.Monoid

import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import           Control.StreamProcessor (SR (..))

import           Language.Forvie.Lexing.Spec
import           Text.Lexeme

highlightLexeme (Lexeme tok _ txt) =
    classOf tok $ H.text txt

classOf tok =
    case lexicalClass tok of
      Comment     -> H.span ! A.class_ "comment"
      Keyword     -> H.span ! A.class_ "keyword"
      Identifier  -> H.span ! A.class_ "identifier"
      Punctuation -> H.span ! A.class_ "punctuation"
      Operator    -> H.span ! A.class_ "operator"
      Constant    -> H.span ! A.class_ "constant"
      Constructor -> H.span ! A.class_ "constructor"
      Whitespace  -> \x -> x

-- | Convert a stream of 'Lexeme's tagged with 'SyntaxHighlight'able
-- tokens into a piece of @pre@formatted HTML. Each non-'WhiteSpace'
-- 'Lexeme' is wrapped in a @\<span class=\"XXX\"\>@ element, where the
-- class depends on the 'Classification' of the Lexeme:
--
--    ['Comment'] becomes @comment@.
--
--    ['Keyword'] becomes @keyword@.
--
--    ['Identifier'] becomes @identifier@.
--
--    ['Punctuation'] becomes @punctuation@.
--
--    ['Operator'] becomes @operator@.
--
--    ['Constant'] becomes @constant@.
-- 
--    ['Constructor'] becomes @constructor@.
--
-- The entire output is then wrapped in a @\<pre\>@ tag.
generateHtml :: SyntaxHighlight tok => SR e (Lexeme tok) H.Html
generateHtml = reader mempty
    where
      reader document = Read f
          where
            f Nothing       = Yield (H.pre document)
            f (Just lexeme) = reader (document `mappend` highlightLexeme lexeme)

