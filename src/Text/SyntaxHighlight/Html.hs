{-# LANGUAGE OverloadedStrings #-}

module Text.SyntaxHighlight.Html
    ( generateHtml )
    where

-- FIXME: instead of assuming that the whitespace (and comments) have
-- been left in the lexeme stream, compute the necessary advance from
-- the position information??

import           Data.Monoid

import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import           Control.StreamProcessor (SR (..))

import           Text.LexicalSpecification
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
      Whitespace  -> \x -> x

generateHtml :: SyntaxHighlight tok => SR e (Lexeme tok) H.Html
generateHtml = reader mempty
    where
      reader document = Read f
          where
            f Nothing       = Yield (H.pre document)
            f (Just lexeme) = reader (document `mappend` highlightLexeme lexeme)
