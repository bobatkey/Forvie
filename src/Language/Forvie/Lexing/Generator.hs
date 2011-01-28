{-# LANGUAGE TypeSynonymInstances,
             TemplateHaskell,
             FlexibleContexts #-}

-- |
-- Module         :  Language.Forvie.SyntaxHighlight.Html
-- Copyright      :  Robert Atkey 2011
-- License        :  BSD3
--
-- Maintainer     :  Robert.Atkey@cis.strath.ac.uk
-- Stability      :  experimental
-- Portability    :  unknown
--
-- This module realises 'LexicalSpecifications' as stream processors
-- that transform streams of 'Char's into streams of 'Lexemes'
-- according to the specification.
--
-- There are two possible ways to realise a specification:
--
--   1. At runtime, simulating the 'DFA.DFA' constructed from a
--      'LexicalSpecification'. This is done by the 'lexerSP'
--      function.
--
--   2. At compile time, compiling the 'DFA.DFA' constructed from a
--      'LexicalSpecification' directly into Haskell code, which the
--      Haskell compiler can then optimise. Obviously, this means the
--      lexical specification must be fixed at compile time. This is
--      done by the 'lexerSPStatic' function.
--
-- This module re-exports the "Text.Lexeme" module which provides a
-- data type and associated functions for representing individual
-- Lexemes from a piece of text.

module Language.Forvie.Lexing.Generator
    ( lexerSP 
    , lexerSPStatic
    , LexingError (..)
    , exceptIgnorable
    , module Text.Lexeme
    )
    where

import           Control.Category ((>>>))
import qualified Control.StreamProcessor as SP
import           Control.StreamProcessor.Positions
import           Control.StreamProcessor.Rewindable
import           Text.Position (Position, Span (Span))
import qualified Data.DFA as DFA
import qualified Data.Text as T
import           Data.DFA.TH
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Language.Forvie.Lexing.Spec
import           Text.Lexeme

-- FIXME: Pass the partially completed lexeme's text to lexing errors.

-- | A class capturing the kinds of error that can occur during
-- lexing. A simple 'String' instance is provided that gives a human
-- readable error message.
class LexingError e where
    -- | End of stream has occured and no complete lexeme has been
    -- found.
    lexingErrAtEOS   :: e
    -- | A character has been encountered that is not recognised as
    -- part of any possible lexeme.
    lexingErrOnInput :: Char -> Position -> e 

instance LexingError String where
    lexingErrAtEOS = "Lexing error at End of Stream"
    lexingErrOnInput c lexeme = "Lexing error on input " ++ show c ++ "; current lexeme: " ++ show lexeme

{------------------------------------------------------------------------------}
data PartialLexeme = PartialLexeme { pLexemeStart        :: Position
                                   , pLexemeTextReversed :: String
                                   }

addToLexeme Nothing                    c p = PartialLexeme p [c]
addToLexeme (Just (PartialLexeme p s)) c _ = PartialLexeme p (c:s)

completeLexeme (PartialLexeme p s) p' tok = Just (Lexeme tok (Span p p') (T.pack $ reverse s))

-- | Dynamically construct a stream processor that transforms a stream
-- of 'Char's into a stream of 'Lexeme's. The DFA constructed from the
-- lexical specification is simulated at runtime.
--
-- The generated lexer annotations each of the characters in the input
-- with position information, which is preserved in the extents
-- attached to each emitted 'Lexeme'. This information starts at
-- 'initPos', i.e. character number 0, line number 1 and column number
-- 0. The column number is incremented for every non-newline (@\\n@)
-- character, and the line number is incremented for every
-- newline. When the line number is incremented, the column number is
-- reset to 0.
lexerSP :: (LexingError e, Ord tok) =>
           CompiledLexSpec tok ->
           SP.SP e Char (Lexeme tok)
lexerSP l = addPositions >>> (rewindableToSP $ go initState)
    where
      dfa = lexSpecDFA l
      
      initState = (0, Nothing, Nothing) -- (DFA state, Partial lexeme, Current match)

      go state = Get $ processInput state

      processInput (_,        Nothing, Nothing) Nothing = EOS
      processInput (_,        Just _,  current) Nothing = emit current Nothing
      processInput (dfaState, lexeme,  current) input@(Just (c,p))
          = let lexeme' = addToLexeme lexeme c p
            in case DFA.transition dfa dfaState c of
                 DFA.Accepting t newState -> Mark $ go (newState, Just lexeme', Just (completeLexeme lexeme' p t))
                 DFA.Error                -> emit current input
                 DFA.Change newState      -> go (newState, Just lexeme', current)

      emit Nothing              Nothing      = Error lexingErrAtEOS
      emit Nothing              (Just (c,p)) = Error $ lexingErrOnInput c p
      emit (Just Nothing)       _            = Rewind $ go initState
      emit (Just (Just lexeme)) _            = Put lexeme $ Rewind $ go initState

-- | Statically construct a lexer, using Template Haskell. The
-- generated lexer has the same interface as the run-time generated
-- lexers from the 'lexerSP' function.
lexerSPStatic :: Lift tok => CompiledLexSpec tok -> ExpQ
lexerSPStatic spec =
    [| let initState = (0, Nothing, Nothing)

           go state = Get $ processInput state

           processInput (_,        Nothing, Nothing) Nothing = EOS
           processInput (_,        Just _,  current) Nothing = emit current Nothing
           processInput (dfaState, lexeme,  current) input@(Just (c,p))
               = let lexeme' = addToLexeme lexeme c p
                 in case $(makeTransitionFunction dfa) dfaState c of
                      DFA.Accepting t newState -> Mark $ go (newState, Just lexeme', Just (completeLexeme lexeme' p t))
                      DFA.Error                -> emit current input
                      DFA.Change newState      -> go (newState, Just lexeme', current)

           emit Nothing              Nothing      = Error lexingErrAtEOS
           emit Nothing              (Just (c,p)) = Error $ lexingErrOnInput c p
           emit (Just Nothing)       _            = Rewind $ go initState
           emit (Just (Just lexeme)) _            = Put lexeme $ Rewind $ go initState
        in
          addPositions >>> (rewindableToSP $ go initState)
     |]
    where
      dfa = lexSpecDFA spec

-- | A stream processor that filters out 'Ignore'able lexemes in a
-- stream. Useful to place between the lexer generated by one of the
-- functions above and the parser.
exceptIgnorable :: SP.SP e (Lexeme (Action tok)) (Lexeme tok)
exceptIgnorable = SP.filterSP f
    where f (Lexeme (Emit t) p s)   = Just (Lexeme t p s)
          f (Lexeme (Ignore _) _ _) = Nothing

