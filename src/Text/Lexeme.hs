module Text.Lexeme
    ( Lexeme(..) )
    where

import qualified Data.Text as T
import           Text.Position (Span)

data Lexeme tok = Lexeme { lexemeTok  :: tok
                         , lexemePos  :: Span
                         , lexemeText :: T.Text
                         }
                deriving (Eq, Ord, Show)

