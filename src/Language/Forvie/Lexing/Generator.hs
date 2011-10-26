{-# LANGUAGE TypeSynonymInstances,
             TemplateHaskell,
             FlexibleContexts #-}

-- |
-- Module         :  Language.Forvie.Lexing.Generator
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
    , lexText
    , lexTextStatic
    , lexStringStatic
    , LexingError (..)
    , exceptIgnorable
    , module Text.Lexeme
    )
    where

import           Control.Category ((>>>))
import qualified Control.StreamProcessor as SP
import           Control.StreamProcessor.Positions
import           Control.StreamProcessor.Rewindable
import           Text.Position (Position, Span (Span), initPos)
import qualified Data.DFA as DFA
import qualified Data.Text as T
import           Data.Maybe (fromMaybe)
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

addToLexeme :: Maybe PartialLexeme -> Char -> Position -> PartialLexeme
addToLexeme Nothing                    c p = PartialLexeme p [c]
addToLexeme (Just (PartialLexeme p s)) c _ = PartialLexeme p (c:s)

completeLexeme :: PartialLexeme -> Position -> tok -> Maybe (Lexeme tok)
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
      processInput (_,        Nothing, Just _)  Nothing = error "Impossible state at end of input" -- FIXME
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

lexText :: (Ord tok) =>
           CompiledLexSpec tok ->
           T.Text ->
          [Lexeme tok]
lexText l = go initState
    where
      dfa = lexSpecDFA l

      initState = (0, Nothing) -- (DFA state, Pointer to start of current lexeme, Current match)

      go (dfaState, matcher) txt =
          case T.uncons txt of
            Nothing ->
                case matcher of
                  Nothing                 -> []
                  Just (ltxt, _, current) -> emit ltxt current Nothing
            Just (c,rst) ->
                let (ltxt, k, current) = fromMaybe (txt, 0, Nothing) matcher
                in case DFA.transition dfa dfaState c of
                     DFA.Accepting t newState -> go (newState, Just (ltxt, k+1, Just (k+1,t))) rst
                     DFA.Error                -> emit ltxt current (Just c)
                     DFA.Change newState      -> go (newState, Just (ltxt, k+1, current)) rst

      emit ltxt Nothing      Nothing  = error $ "Lexing error at end of input"
      emit ltxt Nothing      (Just c) = error $ "Lexing error on input " ++ show c
      emit ltxt (Just (k,t)) _        = Lexeme t (Span initPos initPos) lexeme : go initState rest
          where (lexeme, rest) = T.splitAt k ltxt

lexTextStatic :: Lift tok => CompiledLexSpec tok -> ExpQ
lexTextStatic spec =
    [| let initState = (0, Nothing) -- (DFA state, Pointer to start of current lexeme, Current match)

           transitionFunc = $(makeTransitionFunction dfa)

           go (dfaState, matcher) txt =
               case T.uncons txt of
                 Nothing ->
                     case matcher of
                       Nothing                 -> []
                       Just (ltxt, _, current) -> emit ltxt current Nothing
                 Just (c,rst) ->
                     let (ltxt, k, current) = fromMaybe (txt, 0, Nothing) matcher
                     in case transitionFunc dfaState c of
                          DFA.Accepting t newState -> go (newState, Just (ltxt, k+1, Just (k+1,t))) rst
                          DFA.Error                -> emit ltxt current (Just c)
                          DFA.Change newState      -> go (newState, Just (ltxt, k+1, current)) rst

           emit ltxt Nothing      Nothing  = error $ "Lexing error at end of input"
           emit ltxt Nothing      (Just c) = error $ "Lexing error on input " ++ show c
           emit ltxt (Just (k,t)) _        = Lexeme t (Span initPos initPos) lexeme : go initState rest
               where (lexeme, rest) = T.splitAt k ltxt
       in
         go initState 
     |]
    where dfa = lexSpecDFA spec

lexStringStatic :: Lift tok => CompiledLexSpec tok -> ExpQ
lexStringStatic spec =
    [| let initState = (0, Nothing) -- (DFA state, Pointer to start of current lexeme, Current match)

           go (dfaState, matcher) txt =
               case txt of
                 [] ->
                     case matcher of
                       Nothing                 -> []
                       Just (ltxt, _, current) -> emit ltxt current Nothing
                 (c:rst) ->
                     let (ltxt, k, current) = fromMaybe (txt, 0, Nothing) matcher
                     in case $(makeTransitionFunction dfa) dfaState c of
                          DFA.Accepting t newState -> go (newState, Just (ltxt, k+1, Just (k+1,t))) rst
                          DFA.Error                -> emit ltxt current (Just c)
                          DFA.Change newState      -> go (newState, Just (ltxt, k+1, current)) rst

           emit ltxt Nothing      Nothing  = error $ "Lexing error at end of input"
           emit ltxt Nothing      (Just c) = error $ "Lexing error on input " ++ show c
           emit ltxt (Just (k,t)) _        = Lexeme t (Span initPos initPos) (T.pack lexeme) : go initState rest
               where (lexeme, rest) = splitAt k ltxt
       in
         go initState 
     |]
    where dfa = lexSpecDFA spec