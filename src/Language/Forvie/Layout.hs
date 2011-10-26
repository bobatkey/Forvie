{-# LANGUAGE TemplateHaskell,
             OverloadedStrings,
             TypeSynonymInstances #-}

module Language.Forvie.Layout
    ( NewlineOr (..)
    , Layout (..)
    , LayoutError (..)
    , layout
    )
    where

import Language.Forvie.Lexing.Spec
import Text.Lexeme
import Text.Position
import Control.StreamProcessor
import Language.Haskell.TH.Syntax

-- Do an implementation of layout as per the Haskell standard:
--  http://www.haskell.org/onlinereport/syntax-iso.html

-- See also:
--  http://www.bitc-lang.org/docs/bitc/layout.html

-- 1. A processor that produces the stream expected by the layout algorithm
-- 2. The context-sensitive layout processor

-- Ignore "Note 5" about recovering from parse errors, because it is wicked

-- This algorithm isn't shy about inserting semicolons, so the
-- downstream parser should be able to cope with empty bits between semicolons

-- FIXME: document the position information that appears in the synthetic lexemes

data NewlineOr tok
    = Token tok
    | Newline
    deriving (Ord, Eq, Show)

instance Lift tok => Lift (NewlineOr tok) where
    lift (Token tok) = [| Token $(lift tok) |]
    lift Newline     = [| Newline |]

instance SyntaxHighlight tok => SyntaxHighlight (NewlineOr tok) where
    lexicalClass (Token t) = lexicalClass t
    lexicalClass Newline   = Whitespace

--------------------------------------------------------------------------------
class Eq tok => Layout tok where
    lbrace      :: tok
    rbrace      :: tok
    semicolon   :: tok
    blockOpener :: tok -> Bool

--------------------------------------------------------------------------------
data WithLayout a
    = LLexeme     (Lexeme a)
    | IndentCurly Span Int
    | IndentAngle Span Int
      deriving (Show, Eq, Ord)

-- takes a stream of lexemes with explicit newlines, and outputs a
-- stream without newlines, but with layout directives
insertLayout :: Layout tok => SP e (Lexeme (NewlineOr tok)) (WithLayout tok)
insertLayout = afterBlockOpen
    where
      eos = {- Put (IndentCurly 0) -} EOS

      go = Get go'
          where
            go' Nothing                      = eos
            go' (Just (Lexeme Newline _ _))  = afterNewline
            go' (Just (Lexeme (Token t) p s))
                = Put (LLexeme (Lexeme t p s)) $
                  if blockOpener t then afterBlockOpen else go

      afterBlockOpen = Get afterBlockOpen'
          where
            afterBlockOpen' Nothing                        = eos
            afterBlockOpen' (Just (Lexeme Newline _ _))    = afterBlockOpen
            afterBlockOpen' (Just (Lexeme (Token t) p s))
                = if t == lbrace then
                      Put (LLexeme (Lexeme t p s)) $ go
                  else
                      Put (IndentCurly p (posColumnNum (regionLeft p) + 1)) $
                      Put (LLexeme (Lexeme t p s)) $
                      if blockOpener t then afterBlockOpen else go

      afterNewline = Get afterNewline'
          where
            afterNewline' Nothing = eos
            afterNewline' (Just (Lexeme Newline _ _)) = afterNewline
            afterNewline' (Just (Lexeme (Token t) p s))
                = Put (IndentAngle p (posColumnNum (regionLeft p) + 1)) $
                  Put (LLexeme (Lexeme t p s)) $
                  if blockOpener t then afterBlockOpen else go

class LayoutError e where
    layoutError :: Span -> e

instance LayoutError String where
    layoutError s = "Layout Error"

computeLayout :: (LayoutError e, Layout tok) =>
                 SP e (WithLayout tok) (Lexeme tok)
computeLayout = go []
    where
      -- FIXME: why these strings?
      semicolonLexeme p = Lexeme semicolon p ";"
      lbraceLexeme p = Lexeme lbrace p "{"
      rbraceLexeme p = Lexeme rbrace p "}"

      go stack = Get $ go' stack
          where
            go' ms     (Just (IndentAngle p n))
                = case ms of
                    (m:ms) | m == n -> Put (semicolonLexeme p) $ go (m:ms)
                           | n < m  -> Put (rbraceLexeme p) $ go' ms (Just (IndentAngle p n))
                    ms              -> go ms
            go' ms     (Just (IndentCurly p n))
                = case ms of
                    (m:ms) | n > m -> Put (lbraceLexeme p) $ go (n:m:ms)
                    []     | n > 0 -> Put (lbraceLexeme p) $ go [n]
                    ms             -> Put (lbraceLexeme p) $ Put (rbraceLexeme p) $ go' ms (Just (IndentAngle p n))
            go' (0:ms) (Just (LLexeme l@(Lexeme t p s)))
                | t == rbrace      = Put l $ go ms
            go' ms     (Just (LLexeme (Lexeme t p _)))
                | t == rbrace      = Error $ layoutError p
            go' ms     (Just (LLexeme l@(Lexeme t p s)))
                | t == lbrace      = Put l $ go (0:ms)
            go' ms     (Just (LLexeme l))
                = Put l $ go ms
            go' []     Nothing
                = EOS
            go' (m:ms) Nothing
                = Put (rbraceLexeme (Span initPos initPos)) $ go' ms Nothing -- FIXME: location should be 'at end of input'

layout :: (LayoutError e, Layout tok) =>
          SP e (Lexeme (NewlineOr tok)) (Lexeme tok)
layout = insertLayout >>> computeLayout
