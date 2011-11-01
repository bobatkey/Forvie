-- |
-- Module         : Language.Forvie.Lexing.Text
-- Copyright      : Robert Atkey 2011
-- License        : BSD3
--
-- Maintainer     : Robert.Atkey@cis.strath.ac.uk
-- Stability      : experimental
-- Portability    : unknown
--
-- Functions for turning `Data.Text.Text` into a stream of lexemes,
-- according to some lexical specification.

module Language.Forvie.Lexing.Text
    ( lex
    , ErrorHandler (..) )
    where

import           Prelude hiding (lex)
import qualified Data.Text as T
import           Data.MonadicStream (Stream (..), StreamStep (..))
import qualified Data.DFA as DFA
import           Language.Forvie.Lexing.Spec (CompiledLexSpec (..))
import           Text.Lexeme (Lexeme (..))
import           Text.Position (Span (Span), initPos, updatePos, Position)

{------------------------------------------------------------------------------}
data PositionWith a = At { dataOf     :: a
                         , positionOf :: Position
                         }

uncons :: PositionWith T.Text  -> Maybe (PositionWith Char, PositionWith T.Text)
uncons (text `At` position) =
    case T.uncons text of
      Nothing       -> Nothing
      Just (c,rest) -> Just ( c `At` position
                            , rest `At` (position `updatePos` c) )

{------------------------------------------------------------------------------}
data CurrentLexeme tok
    = CurrentLexeme { curLexemeText  :: !(PositionWith T.Text)
                    , curLexemeLen   :: !Int
                    , curLexemeMatch :: !(CurrentMatch tok)
                    }

data CurrentMatch tok
    = NoMatch
    | Match !Int !tok !Position !(PositionWith T.Text)

advance :: CurrentLexeme tok -> CurrentLexeme tok
advance lexeme = lexeme { curLexemeLen = curLexemeLen lexeme + 1 }

(+.) :: CurrentLexeme tok -> (PositionWith tok, PositionWith T.Text) -> CurrentLexeme tok
lexeme +. (tok `At` pos, rest) =
    lexeme { curLexemeMatch = Match (curLexemeLen lexeme) tok pos rest }

initLexeme :: PositionWith T.Text -> CurrentLexeme tok
initLexeme text = CurrentLexeme text 0 NoMatch

{------------------------------------------------------------------------------}
data ErrorHandler m tok = OnError { onError :: Maybe (Char, Position) -> m tok }

{------------------------------------------------------------------------------}
lex :: (Ord tok, Monad m) =>
       CompiledLexSpec tok
    -> ErrorHandler m tok
    -> T.Text
    -> Stream m (Lexeme tok)
lex lexSpec errorHandler text = go (text `At` initPos)
    where
      dfa = lexSpecDFA lexSpec
      
      go text =
          Stream $ beforeLexeme text

      beforeLexeme text =
          case uncons text of
            Nothing   -> return StreamEnd
            Just step -> onChar 0 step (initLexeme text)

      inLexeme dfaState lexeme text =
          case uncons text of
            Nothing   -> emit lexeme Nothing
            Just step -> onChar dfaState step lexeme

      onChar q input@(c `At` position, rest) lexeme =
          case DFA.transition dfa q c of
            DFA.Accepting t q' -> inLexeme q' (advance lexeme +. (t `At` position, rest)) rest
            DFA.Error          -> emit lexeme (Just input)
            DFA.Change q'      -> inLexeme q' (advance lexeme) rest

      emit lexeme input =
          case curLexemeMatch lexeme of
            NoMatch ->
                do let input' = case input of Nothing -> Nothing; Just (c `At` p, _) -> Just (c,p)
                   tok <- errorHandler `onError` input'
                   return (StreamElem (Lexeme tok span text) $ go rest)
                where
                  endPos = case input of
                             Nothing            -> positionOf $ curLexemeText lexeme -- FIXME: this is wrong!
                             Just (_ `At` p, _) -> p
                  rest   = case input of
                             Nothing        -> T.empty `At` initPos -- FIXME
                             Just (_, rest) -> rest
                  length = curLexemeLen lexeme + case input of Nothing -> 0; Just _ -> 1
                  span   = Span (positionOf $ curLexemeText lexeme) endPos
                  text   = T.take length (dataOf $ curLexemeText lexeme)
            Match length tok endPos rest ->
                return (StreamElem (Lexeme tok span text) $ go rest)
                where
                  span = Span (positionOf $ curLexemeText lexeme) endPos
                  text = T.take length (dataOf $ curLexemeText lexeme)
