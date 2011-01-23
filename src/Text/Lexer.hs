{-# LANGUAGE TypeSynonymInstances,
             TemplateHaskell,
             FlexibleContexts #-}

module Text.Lexer
    ( LexingError (..)
    , lexerSP 
    , lexerSPStatic
    , exceptIgnorable
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
import           Text.LexicalSpecification
import           Text.Lexeme

class LexingError e where
    lexingErrAtEOS   :: e
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

--completeLexeme (PartialLexeme p s) p' (Ignore _) = Nothing
completeLexeme (PartialLexeme p s) p' tok = Just (Lexeme tok (Span p p') (T.pack $ reverse s))

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

exceptIgnorable :: SP.SP e (Lexeme (Action tok)) (Lexeme tok)
exceptIgnorable = SP.filterSP f
    where f (Lexeme (Emit t) p s)   = Just (Lexeme t p s)
          f (Lexeme (Ignore _) _ _) = Nothing

