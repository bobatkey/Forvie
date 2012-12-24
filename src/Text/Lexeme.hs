-- |
-- Module             :  Text.Lexeme
-- Copyright          :  Robert Atkey 2012
-- License            :  BSD3
--
-- Maintainer         :  bob.atkey@gmail.com
-- Stability          :  experimental
-- Portability        :  unknown
--
-- Representation of lexemes, pieces of text tagged with semantic
-- tokens.

module Text.Lexeme
    ( Lexeme(..) )
    where

import qualified Data.Text as T
import           Text.Position (Span, Regioned (..))

-- | A lexeme is an annotated piece of text from a larger body of
-- text, consisting of three parts:
--
--  * A token, indicating the semantic meaning that has been given to
--  this piece of text.
--
--  * A 'Span' indicating the position of this lexeme in the larger
--  body of text.
--
--  * The piece of text itself
data Lexeme tok = Lexeme { lexemeTok  :: !tok
                         , lexemePos  :: !Span
                         , lexemeText :: !T.Text
                         }
                deriving (Eq, Ord, Show)

instance Regioned (Lexeme tok) where
    regionLeft  = regionLeft . lexemePos
    regionRight = regionRight . lexemePos
