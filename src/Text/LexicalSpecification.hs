{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

-- |
-- Module      : Text.LexicalSpecification
-- Copyright   : Robert Atkey 2011
-- License     : BSD3
--
-- Maintainer  : Robert.Atkey@cis.strath.ac.uk
-- Stability   : experimental
-- Portability : unknown
--
-- Specification of the lexical structure of programming languages,
-- and its compilation to deterministic finite automaton.
--
-- A lexical specification consists of a list of regular expressions
-- with associated tokens, with the intended semantics that entries
-- earlier in the list take precedence over later ones.

module Text.LexicalSpecification
    ( -- * Specification of Lexical Structure
      LexicalSpecification (..)
    , module Data.Regexp
    , module Data.CharSet
      
      -- * Compilation
    , CompiledLexSpec
    , compileLexicalSpecification
    , lexSpecDFA
      
      -- * Useful token types
    , Classification (..)
    , SyntaxHighlight (..)
    , Action (..)
    , ClassifiedToken (..)
    )
    where

import           Language.Haskell.TH.Syntax
import qualified Data.DFA as DFA
import           Data.Regexp
import           Data.CharSet

-- | A 'LexicalSpecification' represents the specification of the
-- lexical structure of some language. A specification consists of a
-- list of regular expressions (of type 'Regexp') each with an
-- associated lexical value. In the case of overlap, the intended
-- semantics is that entries earlier in the list take precedence.
type LexicalSpecification tok = [(Regexp Char, tok)]


-- | A rough classification of lexemes, used for syntax highlighting.
data Classification
    = Comment      -- ^ Comments
    | Keyword      -- ^ A keyword of the language
    | Identifier   -- ^ An identifier of the language
    | Punctuation  -- ^ Punctuation, such as parentheses or colons
    | Whitespace   -- ^ Whitespace: spaces, tabs, newlines
    | Constant     -- ^ Constants, usually strings and integers
    | Operator     -- ^ Usually infix operators
    deriving (Eq, Ord, Show)

instance Lift Classification where
    lift Comment     = [| Comment |]
    lift Keyword     = [| Keyword |]
    lift Identifier  = [| Identifier |]
    lift Punctuation = [| Punctuation |]
    lift Whitespace  = [| Whitespace |]
    lift Operator    = [| Operator |]
    lift Constant    = [| Constant |]

-- | Instances of this class are able to classify themselves into a
-- specific 'Classification' for the purposes of syntax highlighting.
class SyntaxHighlight tok where
    lexicalClass :: tok -> Classification

instance SyntaxHighlight Classification where
    lexicalClass = id

data Action tok
    = Emit   tok
    | Ignore Classification
    deriving (Eq, Ord, Show)

instance SyntaxHighlight tok => SyntaxHighlight (Action tok) where
    lexicalClass (Emit tok)   = lexicalClass tok
    lexicalClass (Ignore typ) = lexicalClass typ

instance Lift tok => Lift (Action tok) where
    lift (Emit tok)   = [| Emit $(lift tok) |]
    lift (Ignore typ) = [| Ignore $(lift typ) |]

data ClassifiedToken kw ident punct op const
    = TokKeyword kw
    | TokIdentifier ident
    | TokPunctuation punct
    | TokOperator op
    | TokConstant const
    deriving (Eq, Ord, Show)

instance SyntaxHighlight (ClassifiedToken kw ident punct op const) where
    lexicalClass (TokKeyword _)     = Keyword
    lexicalClass (TokIdentifier _)  = Identifier
    lexicalClass (TokPunctuation _) = Punctuation
    lexicalClass (TokOperator _)    = Operator
    lexicalClass (TokConstant _)    = Constant

-- | The 'CompiledLexSpec' type represents a lexical specification
-- that has been compiled into a DFA (Deterministic Finite Automaton).
newtype CompiledLexSpec tok
    = LS { -- | Extract the DFA (Deterministic Finite Automaton) from a compiled lexical specification.
           lexSpecDFA :: DFA.DFA Char tok
      }

-- | Compile a lexical structure specification into a DFA
-- (Deterministic Finite Automaton).
--
-- Note that this compilation step can be relatively expensive, so it
-- is recommended to ensure that all uses of a compiled lexer
-- specification are shared so that the compilation step is only
-- executed once.
compileLexicalSpecification :: Ord tok => LexicalSpecification tok -> CompiledLexSpec tok
compileLexicalSpecification regexps = LS $ DFA.makeDFA regexps
