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
    ( lexer )
    where

import           Control.Monad.Error
import qualified Data.Text as T
import           Data.MonadicStream (Stream (..), StreamStep (..))
import qualified Data.DFA as DFA
import           Language.Forvie.Lexing.Spec (CompiledLexSpec (..))
import           Language.Forvie.Lexing.Error (LexingError (..))
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
    | Match !Int !tok !Position

advance :: CurrentLexeme tok -> CurrentLexeme tok
advance lexeme = lexeme { curLexemeLen = curLexemeLen lexeme + 1 }

(+.) :: CurrentLexeme tok -> PositionWith tok -> CurrentLexeme tok
lexeme +. (tok `At` pos) =
    lexeme { curLexemeMatch = Match (curLexemeLen lexeme) tok pos }

initLexeme :: PositionWith T.Text -> CurrentLexeme tok
initLexeme text = CurrentLexeme text 0 NoMatch

{------------------------------------------------------------------------------}
lexer :: (Ord tok, LexingError e, MonadError e m) =>
         CompiledLexSpec tok
      -> T.Text
      -> Stream m (Lexeme tok)
lexer lexSpec text = go (text `At` initPos)
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

      onChar q (c `At` position, rest) lexeme =
          case DFA.transition dfa q c of
            DFA.Accepting t q' -> inLexeme q' (advance lexeme +. (t `At` position)) rest
            DFA.Error          -> emit lexeme (Just (c,position))
            DFA.Change q'      -> inLexeme q' (advance lexeme) rest

      emit lexeme input =
          case curLexemeMatch lexeme of
            NoMatch ->
                throwError (lexingErr input)
            Match length tok endPos ->
                return (StreamElem (Lexeme tok span text) $ go (rest `At` endPos))
                where
                  span        = Span (positionOf $ curLexemeText lexeme) endPos
                  (text,rest) = T.splitAt length (dataOf $ curLexemeText lexeme)
